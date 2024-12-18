(ns clj-arsenal.madam 
  (:require
    [clj-arsenal.log :refer [log]]
    [clj-arsenal.check :refer [check expect]]))

(declare calc-patch)

(defn- assoc-op
  [basis & {:as other-map}]
  {:pre [(map? basis)]}
  (mapv persistent!
    (reduce-kv
      (fn [[merged changed-paths] k v]
        (let [current-val (get basis k)]
          (if (identical? current-val v)
            [merged changed-paths]
            [(assoc! merged k v)
             (conj! changed-paths [k])])))
      [(transient basis) (transient #{})]
      other-map)))

(defn merge-op
  [basis & maps]
  {:pre [(every? #(or (nil? %) (map? %)) maps)]}
  [(apply merge basis maps)
   (mapcat (fn [m] (map vector (keys m))) maps)])

(defn- clear-op
  [basis & other-vals]
  {:pre [(or
           (set? basis)
           (map? basis)
           (and (vector? basis) (every? int? other-vals)))]}
  (cond
    (set? basis)
    [(apply disj basis other-vals) #{[]}]

    (map? basis)
    [(apply dissoc basis other-vals)
     (set (map vector other-vals))]

    (vector? basis)
    (let [num-to-clear (count other-vals)]
      (case num-to-clear
        0
        [basis #{}]

        1
        (let [idx (first other-vals)]
          [(into (subvec basis 0 idx) (subvec basis (inc idx)))
           #{[idx]}])

        :else
        [(let [vals-to-clear (set other-vals)]
           (->> basis
             (keep-indexed
               (fn [idx x]
                 (when-not (contains? vals-to-clear idx)
                   x)))
             vec))
         (set (map vector other-vals))]))))

(defn- conj-op
  [basis & other-vals]
  {:pre [(coll? basis)]}
  [(apply conj basis other-vals)
   (cond-> #{[]}
     (vector? basis)
     (into (map vector (range (count basis) (+ (count basis) (count other-vals)))))

     (map? basis)
     (into (map (comp vector first) other-vals)))])

(defn- into-op
  [basis other-val]
  {:pre [(coll? basis)]}
  [(into basis other-val)
   (cond-> #{[]}
     (vector? basis)
     (into (map vector (range (count basis) (+ (count basis) (count other-val)))))

     (map? basis)
     (into (map (comp vector first) other-val)))])

(defn- call-op
  [basis f & args]
  {:pre [(ifn? f)]}
  [(apply f basis args)
   #{[]}])

(defn- value-op
  [_basis other-val]
  [other-val
   #{[]}])

(def default-operators
  {:assoc assoc-op
   :merge merge-op
   :clear clear-op
   :conj conj-op
   :into into-op
   :call call-op
   :value value-op})

(defn madam?
  [x]
  (boolean (some-> x meta ::madam)))

(defn madam
  [x & {:keys [operators extra-operators validator]}]
  {:pre [(map? x)]}
  (let [!atom (atom (vary-meta x assoc ::watches {:tree {} :watchers {}})
                :validator (if (ifn? validator) #(and (map? %) (validator %)) map?)
                :meta {::madam true
                       ::operators (-> (or default-operators operators)
                                     (merge  extra-operators))})]
    (add-watch !atom ::madam-watch
      (fn [_ _ old-val new-val]
        (let [{changed-paths ::changed-paths
               affected-watchers ::affected-watchers} (meta new-val)]
          (doseq [{:keys [f path]}
                  (or affected-watchers
                    (-> !atom meta ::watches :watchers vals))]
            (try
              (f
                (get-in old-val path)
                (get-in new-val path)
                (or changed-paths #{[]}))
              (catch #?(:clj Exception :cljd Exception :cljs :default) ex
                (log :error :msg "Error in madam watcher" :ex ex)))))))))

(defn calc-patch
  [operators basis patch]
  (reduce
    (fn [[patching changed-paths] {:keys [path change fnil subpatch]}]
      (let [[op-fn args] (if (some? subpatch)
                           [(partial calc-patch operators) [subpatch]]
                           [(get operators (first change)) (rest change)])
            _ (assert (ifn? op-fn))
            _ (assert (vector? path))
            inner-basis (get-in patching path)
            inner-basis (if (some? inner-basis) inner-basis fnil)
            [new-inner-basis inner-changed-paths] (apply op-fn inner-basis args)]
        [(if (seq path)
           (assoc-in patching path new-inner-basis)
           new-inner-basis)
         (concat changed-paths (map #(into path %) inner-changed-paths))]))
    [basis nil]
    (if (map? patch)
      [patch]
      patch)))

(defn- watched-sub-paths
  [watch-tree path]
  (letfn [(collect-all-sub-paths [cur-path watch-node]
            (cons
              cur-path
              (mapcat
                (fn [[k sub-node]]
                  (collect-all-sub-paths (conj cur-path k) sub-node))
                (:sub watch-node))))]
    (collect-all-sub-paths path
      (if (empty? path)
        watch-tree
        (get-in (:sub watch-tree) (interpose :sub path))))))

(defn- super-paths
  [path]
  (map #(subvec path 0 %) (range 0 (count path))))

(defn- all-paths-affected-by-change-at
  [watch-tree path]
  (concat
    (super-paths path)
    (watched-sub-paths watch-tree path)))

(defn- path->watch-node-path
  [path]
  (if (empty? path)
    [:tree]
    (into [:tree :sub] (interpose :sub path))))

(defn patch!
  [!madam patch & {:keys [when]}]
  {:pre [(madam? !madam)]}
  (let [[old new :as r]
        (swap-vals! !madam
          (fn [basis]
            (if (and (ifn? when) (not (when basis)))
              basis
              (let [watches (-> basis meta ::watches)
                    operators (-> !madam meta ::operators)
                    [patched changed-paths] (calc-patch operators basis patch)
                    changed-paths (set changed-paths)

                    affected-paths
                    (set (mapcat (partial all-paths-affected-by-change-at
                                   (:tree watches))
                           changed-paths))

                    affected-watchers
                    (mapcat
                      (fn [affected-path]
                        (->> (get-in watches (path->watch-node-path affected-path))
                          :watch-keys
                          (map
                            (fn [watch-key]
                              (get-in watches [:watchers watch-key])))))
                      affected-paths)]
                (vary-meta patched assoc
                  ::affected-watchers affected-watchers
                  ::changed-paths changed-paths
                  ::watches watches)))))]
    (when-not (identical? old new)
      r)))

(defn watch
  [!madam k path f]
  {:pre [(madam? !madam) (vector? path)]}
  (let [watch-node-path (path->watch-node-path path)]
    (swap! !madam vary-meta update ::watches
      (fn [watches]
        (-> watches
          (update-in (conj watch-node-path :watch-keys) (fnil conj #{}) k)
          (assoc-in [:watchers k] {:path path :f f})))))
  nil)

(defn unwatch
  [!madam k]
  {:pre [(madam? !madam)]}
  (swap! !madam vary-meta update ::watches
    (fn [watches]
      (if-let [path (some-> watches :watchers (get k) :path)]
        (-> watches
          (update :watchers dissoc k)
          (assoc :tree
            (loop [cur-path (path->watch-node-path path)
                   cur-node (update (get-in watches cur-path) :watch-keys disj k)]
              (if (= [:tree] cur-path)
                cur-node
                (let [parent-path (-> cur-path pop pop)
                      parent-node (as-> (get-in watches parent-path) $
                                    (if (and (empty? (:watch-keys cur-node)) (empty? (:sub cur-node)))
                                      (update $ :sub dissoc (-> cur-path peek))
                                      (assoc-in $ [:sub (-> cur-path peek)] cur-node)))]
                  (recur parent-path parent-node))))))
        watches))))

(check ::affected-paths
  (let [!madam (madam {:x {:y {:z 1}} :m {}})
        !affected-paths (atom #{})]
    (watch !madam ::xyz-path [:x :y :z]
      (fn [& _] (swap! !affected-paths conj [:x :y :z])))
    (watch !madam ::xy-path [:x :y]
      (fn [& _] (swap! !affected-paths conj [:x :y])))
    (watch !madam ::x-path [:x]
      (fn [& _] (swap! !affected-paths conj [:x])))
    (watch !madam ::root-path []
      (fn [& _] (swap! !affected-paths conj [])))
    (watch !madam ::m-path [:m]
      (fn [& _] (swap! !affected-paths conj [:m])))

    (patch! !madam
      {:path [:x :y]
       :change [:value 2]})

    (expect = @!affected-paths #{[] [:x] [:x :y] [:x :y :z]})))

(check ::assoc-op
  (let [orig {:foo "foo"}
        !madam (madam orig)
        [old new] (patch! !madam {:path [] :change [:assoc :foo "FOO" :bar "BAR"]})]
    (expect = old orig)
    (expect = new {:foo "FOO" :bar "BAR"})))

(check ::clear-op
  (let [orig {:foo "foo" :x [1 2]}
        !madam (madam orig)
        [old new] (patch! !madam
                    [{:path [] :change [:clear :foo]}
                     {:path [:x] :change [:clear 1]}])]
    (expect = old orig)
    (expect = new {:x [1]})))

(check ::conj-op
  (let [orig {:x [1]}
        !madam (madam orig)
        [old new] (patch! !madam [{:path [:x] :change [:conj 2 3]}])]
    (expect = old orig)
    (expect = new {:x [1 2 3]})))

(check ::into-op
  (let [orig {:x [1]}
        !madam (madam orig)
        [old new] (patch! !madam [{:path [:x] :change [:into [2 3]]}])]
    (expect = old orig)
    (expect = new {:x [1 2 3]})))

(check ::call-op
  (let [orig {:x 1}
        !madam (madam orig)
        [old new] (patch! !madam [{:path [:x] :change [:call inc]}])]
    (expect = old orig)
    (expect = new {:x 2})))

