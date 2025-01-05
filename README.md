An atom that can be watched for changes under particular paths.
Useful for simple front-end application state stores.

```clojure
(require '[clj-arsenal.madam :as madam])

(def !store (madam/madam {:foo {:bar 2}}))

(madam/watch !store ::my-watch [:foo :bar]
 (fn [old-val new-val _changed-paths]
   (prn :old-val old-val :new-val new-val)))

(madam/patch! !store
  {:path [:foo :bar]
   :change [:call inc]})
; >> :old-val 2 :new-val 3

(madam/unwatch !store ::my-watch)
```

Patching a 'madam' keeps track of which paths the applied operator
could have changed.  The default operators are `:assoc`, `:merge`,
`:clear` (dissoc, disj, remove index from vector), `:conj`, `:into`,
`:call`, `:value` (replace value).

An `:fnil` entry can be provided on the patch, which is substituted
in for the current value if the current value is nil or doesn't exist.

A custom set of operators can be given as a map under the `:operators`
option of the madam constructor.  These operators _replace_ the defaults,
so make sure to merge in `default-operators` to keep them.  The keys of
the operator map are operator names, and the values are functions of the
form `(f basis & args) -> [new-value #{& changed-subpaths}]`.  Where
`basis` is given as either the current value at the patch path, or the
`:fnil` value if the current value is nil or non-existent.  `args` is
given as the remaining items in the `:change` vector, after the operator.
The `new-value` is assoc-ed at the path path, and the `changed-subpaths`
help determine which subpaths (under the patch path) could have been
changed by the operation.

Patches can be given as either a single patch map, or a collection of
patch maps.

Instead of a `:change`, a patch map can specify a `:subpatch`, which
is another patch that'll be applied at the patch path.
