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
