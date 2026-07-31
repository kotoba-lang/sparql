(ns sparql.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [sparql.core :as sparql]))

(defn- iri [v] {:rdf/type :iri :value v})
(defn- lit [v] {:rdf/type :literal :value v})

(def role (iri "role"))
(def name-pred (iri "name"))
(def knows (iri "knows"))

(def quads
  [{:subject (iri "alice") :predicate role :object (lit "admin")}
   {:subject (iri "alice") :predicate name-pred :object (lit "Alice")}
   {:subject (iri "alice") :predicate knows :object (iri "bob")}
   {:subject (iri "bob") :predicate role :object (lit "user")}
   {:subject (iri "bob") :predicate name-pred :object (lit "Bob")}])

(deftest bgp-single-pattern-binds-var
  (let [algebra {:sparql/op :bgp :patterns [['?s role '?r]]}
        results (sparql/select algebra quads)]
    (is (= 2 (count results)))
    (is (some #(= (lit "admin") (get % '?r)) results))))

(deftest bgp-multi-pattern-joins-on-shared-var
  (let [algebra {:sparql/op :bgp
                 :patterns [['?s role (lit "admin")]
                            ['?s name-pred '?n]]}
        results (sparql/select algebra quads)]
    (is (= 1 (count results)))
    (is (= (lit "Alice") (get (first results) '?n)))))

(deftest filter-narrows-results
  (let [algebra {:sparql/op :filter
                 :pred (fn [b] (= (lit "admin") (get b '?r)))
                 :pattern {:sparql/op :bgp :patterns [['?s role '?r]]}}
        results (sparql/select algebra quads)]
    (is (= 1 (count results)))))

(deftest union-combines-both-branches
  (let [algebra {:sparql/op :union
                 :left {:sparql/op :bgp :patterns [['?s role (lit "admin")]]}
                 :right {:sparql/op :bgp :patterns [['?s role (lit "user")]]}}
        results (sparql/select algebra quads)]
    (is (= 2 (count results)))))

(deftest optional-keeps-unmatched-left-rows
  (let [algebra {:sparql/op :optional
                 :left {:sparql/op :bgp :patterns [['?s name-pred '?n]]}
                 :right {:sparql/op :bgp :patterns [['?s knows '?friend]]}}
        results (sparql/select algebra quads)
        bob-row (some #(when (= (iri "bob") (get % '?s)) %) results)]
    (is (= 2 (count results)))
    (is (nil? (get bob-row '?friend)))))

(deftest project-keeps-only-listed-vars
  (let [algebra {:sparql/op :project
                 :vars ['?n]
                 :pattern {:sparql/op :bgp :patterns [['?s name-pred '?n]]}}
        results (sparql/select algebra quads)]
    (is (every? #(= #{'?n} (set (keys %))) results))))

(deftest distinct-dedupes
  (let [algebra {:sparql/op :distinct
                 :pattern {:sparql/op :project
                           :vars ['?r]
                           :pattern {:sparql/op :bgp :patterns [['?s role '?r]]}}}
        results (sparql/select algebra quads)]
    (is (= 2 (count results)))))

(deftest slice-limits-and-offsets
  (let [algebra {:sparql/op :slice
                 :offset 1 :limit 1
                 :pattern {:sparql/op :bgp :patterns [['?s name-pred '?n]]}}
        results (sparql/select algebra quads)]
    (is (= 1 (count results)))))

(deftest ask-reports-match-existence
  (testing "matching pattern"
    (is (true? (sparql/ask {:sparql/op :bgp :patterns [['?s role (lit "admin")]]} quads))))
  (testing "non-matching pattern"
    (is (false? (sparql/ask {:sparql/op :bgp :patterns [['?s role (lit "superadmin")]]} quads)))))

;; --- ORDER BY direction ----------------------------------------------------

(deftest order-by-descending
  (testing "DESC used to be accepted by the parser and then silently sorted
            ASCENDING, because the algebra could only express one direction —
            a wrong answer, not a missing feature"
    (let [rows [{'?n 1} {'?n 3} {'?n 2}]
          asc (sort (sparql/row-comparator '[?n]) rows)
          desc (sort (sparql/row-comparator '[?n] '#{?n}) rows)]
      (is (= [1 2 3] (map '?n asc)))
      (is (= [3 2 1] (map '?n desc))))))

(deftest order-by-mixes-directions-per-var
  (testing "each var carries its own direction, which is the whole reason
            sort-by + juxt could not be kept"
    (let [rows [{'?a 1 '?b "x"} {'?a 1 '?b "y"} {'?a 2 '?b "x"}]
          out (sort (sparql/row-comparator '[?a ?b] '#{?b}) rows)]
      (is (= [[1 "y"] [1 "x"] [2 "x"]] (map (juxt '?a '?b) out))))))

(deftest order-by-tolerates-unbound-and-mixed-types
  (testing "a solution sequence is exactly where mixed types show up — SPARQL
            binds whatever the data holds. compare/1 throws across types, so an
            ORDER BY over a heterogeneous column used to be an exception"
    (let [rows [{'?v "b"} {'?v 2} {} {'?v "a"}]
          out (sort (sparql/row-comparator '[?v]) rows)]
      (is (= 4 (count out)))
      (is (nil? ('?v (first out))) "unbound sorts first, per SPARQL 1.1 §15.1")
      (is (= ["a" "b"] (filter string? (map '?v out))) "and within a type, ordered"))))

(deftest order-by-is-total-so-sorting-never-throws
  (is (some? (sort (sparql/row-comparator '[?v]) [{'?v {:a 1}} {'?v [1]} {'?v "s"}]))))
