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

(defn- cmp
  "Compare two bound values, tolerating unbound (nil) and mixed types.

  `clojure.core/compare` throws across types, and a solution sequence is
  exactly where mixed types show up — SPARQL binds whatever the data holds
  and does not promise homogeneity. Unbound sorts first (SPARQL 1.1 §15.1
  puts unbound before everything), then values are compared inside a type,
  and across types by type name so the order is at least total and stable
  rather than an exception."
  [a b]
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b) 1
    :else
    (let [ta (type a) tb (type b)]
      (if (= ta tb)
        (try (compare a b)
             (catch #?(:clj Exception :cljs :default) _
               (compare (str a) (str b))))
        (compare (str ta) (str tb))))))

(defn row-comparator
  "Comparator over solution maps for `vars`, descending for those in `desc`.

  Written out rather than `sort-by` + `juxt` because that form can only sort
  every key the same direction — which is why `ORDER BY DESC(?x)` used to be
  accepted and then silently sorted ASCENDING. A comparator that cannot express
  the query is worse than one that rejects it."
  ([vars] (row-comparator vars #{}))
  ([vars desc]
   (fn [x y]
     (loop [vs (seq vars)]
       (if-not vs
         0
         (let [v (first vs)
               c (cmp (get x v) (get y v))
               c (if (contains? desc v) (- c) c)]
           (if (zero? c) (recur (next vs)) c)))))))

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
    :order-by (sort (row-comparator (:vars node) (set (:desc node)))
                    (eval-node (:pattern node) quads))
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
