(ns hopf-algebra.linear-combination-plus-test
  (:require [clojure.test :refer :all]
            [hopf-algebra.hopf-algebra :refer [product coproduct antipode to-str HopfAlgebra]]
            [hopf-algebra.linear-combination-plus :as lcp]))



(deftest lc-plus?-test []
  (is (lcp/lcp? {}))

  (is (lcp/lcp? {"x1" {"c1" 55}}))

  (is (not (lcp/lcp? {"x1" 55})))

  )

(defrecord Concatter [content])

(extend-type Concatter
  ;- a simple stub, just implementing a subset of the HopfAlgebra protocol
  HopfAlgebra
  (to-str [x] (:content x))
  (to-latex [x] (:content x))
  (product [a b] { (->Concatter (str (:content a) (:content b))) 1 } ))

(deftest formatting-test []
  (is (= {"yes" {"way" 8}}
         (lcp/+ {"yes" {"way" 1}} {"yes" {"way" 7}})))
  (is (thrown? java.lang.AssertionError
               (lcp/+ {"no" {"way" "way"}} {"jo" {"way" "way"}})))

  ;(is (= {} (lc/remove-zeros {"zero" 0})))
  (is (= {} (lcp/remove-zeros {"zero" {"zero" 0}})))
)

(deftest lc-plus-test
  (is (=
       {"oneone" {"c-1" 12/5}}
       (lcp/apply-linear-function
         (fn [x] { (str x x) 3})
         {"one" {"c-1" 4/5}})))

  (is (=
       {"one" {"c-1" 14}}
       (lcp/apply-linear-function'
         (fn [x] {x {"c-1" 7}})
         {"one" 2})))

  (is (=
       {"two" {"c-1" 4/5}}
       (lcp/apply-linear-function
         (fn [x] (if (= x "one") {} {x 1}))
         {"one" {"c-1" 4/5}
          "two" {"c-1" 4/5}})))

  (is (=
       { "key-1" {"C1" 111}
         "key-2" {"C1" 404} }
       (reduce
         lcp/+
         {}
         [
          { "key-1" {"C1" 11} }
          { "key-2" {"C1" 404} }
          { "key-1" {"C1" 100} }
          ])))
  (is (=
       { "key-1" {"value" 11},
         "key-2" {"value" 202} }
       (lcp/lc->lcp
         { "key-1" 11
           "key-2" 202 }
         "value")))

  (is (= {(->Concatter "hellyeah") {(->Concatter "ab") 440, (->Concatter "Ab") 880}}
         (lcp/*
           10
           {(->Concatter "hell") {(->Concatter "a") 11, (->Concatter "A") 22}}
           {(->Concatter "yeah") {(->Concatter "b") 4}})))
  (is (= {(->Concatter "hellyeahyeah") {(->Concatter "abc") 880}}
         (lcp/*
           2
           {(->Concatter "hell") {(->Concatter "a") 11}}
           {(->Concatter "yeah") {(->Concatter "b") 4}}
           {(->Concatter "yeah") {(->Concatter "c") 10}})))
  )
