(ns sparql.core
  "EDN-first SPARQL algebra: pattern-match/join/filter over rdf.core-shaped
  quads. No SPARQL text-syntax parser here -- callers build the algebra
  tree as plain EDN, same philosophy as rdf/turtle/shacl. A future
  string-syntax parser is a separate, later layer.")

(defn logic-var? [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn- term= [a b]
  (= a b))

(defn- match-component [pat quad-val bindings]
  (cond
    (logic-var? pat)
    (if (contains? bindings pat)
      (when (term= (get bindings pat) quad-val) bindings)
      (assoc bindings pat quad-val))

    :else
    (when (term= pat quad-val) bindings)))

(defn match-triple
  "Try to unify one [s p o] pattern (terms or ?vars) against one quad map
  (:subject/:predicate/:object, rdf.core shape). Returns an extended
  bindings map, or nil on mismatch."
  [[ps pp po] quad bindings]
  (some->> bindings
           (match-component ps (:subject quad))
           (match-component pp (:predicate quad))
           (match-component po (:object quad))))

(defn- bgp
  "Multiple triple patterns, joined left-to-right by shared ?vars.
  Returns the seq of bindings-maps satisfying every pattern."
  [patterns quads]
  (reduce
   (fn [binding-seq pattern]
     (for [bindings binding-seq
           quad quads
           :let [next (match-triple pattern quad bindings)]
           :when next]
       next))
   [{}]
   patterns))

(defn- eval-node [node quads]
  (case (:sparql/op node)
    :bgp      (bgp (:patterns node) quads)
    :filter   (filter (:pred node) (eval-node (:pattern node) quads))
    :join     (for [l (eval-node (:left node) quads)
                     r (eval-node (:right node) quads)
                     :when (every? (fn [[k v]] (= v (get r k v))) l)]
                 (merge l r))
    :union    (concat (eval-node (:left node) quads) (eval-node (:right node) quads))
    :optional (let [lefts (eval-node (:left node) quads)
                    rights (eval-node (:right node) quads)]
                (mapcat
                 (fn [l]
                   (let [matches (keep (fn [r] (when (every? (fn [[k v]] (= v (get r k v))) l)
                                                 (merge l r)))
                                        rights)]
                     (if (seq matches) matches [l])))
                 lefts))
    :project  (map #(select-keys % (:vars node)) (eval-node (:pattern node) quads))
    :distinct (distinct (eval-node (:pattern node) quads))
    :order-by (sort-by (apply juxt (:vars node)) (eval-node (:pattern node) quads))
    :slice    (cond->> (eval-node (:pattern node) quads)
                (:offset node) (drop (:offset node))
                (:limit node)  (take (:limit node)))))

(defn select
  "Execute a SPARQL algebra tree (see the :sparql/op node shapes above)
  against a seq of rdf.core-shaped quads. Returns a seq of bindings maps
  ({'?var term ...})."
  [algebra quads]
  (vec (eval-node algebra quads)))

(defn ask
  "True if the algebra tree matches at least one binding."
  [algebra quads]
  (boolean (seq (eval-node algebra quads))))
