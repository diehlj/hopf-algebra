(ns hopf-algebra.linear-combination-test
  (:require [clojure.test :refer :all]
            [hopf-algebra.hopf-algebra :refer [product coproduct antipode to-str HopfAlgebra]]
            [hopf-algebra.linear-combination :as lc]))


(deftest lc?-test []
  (is (lc/lc? {}))

  (is (lc/lc? {"x1" 22, "x2", -29 }))

  (is (not (lc/lc? {"x1" "s"})))
  )

(defrecord Concatter [content])

(extend-type Concatter
  ;- a simple stub, just implementing a subset of the HopfAlgebra protocol
  HopfAlgebra
  (to-str [x] (:content x))
  (to-latex [x] (:content x))
  (product [a b] { (->Concatter (str (:content a) (:content b))) 1 } ))

(deftest formatting-test []
  (is (= "" (lc/format-number 0 1)))
  (is (= "-" (lc/format-number 0 -1)))
  (is (= "8.9" (lc/format-number 0 8.9)))
  (is (= "-8.9" (lc/format-number 0 -8.9)))

  (is (= "+8.9" (lc/format-number 7 8.9)))
  (is (= "-8.9" (lc/format-number 7 -8.9)))

  (is (= "+" (lc/format-number 7 1)))
  (is (= "-17" (lc/format-number 0 -17)))

  (is (=
       "-17 hell \u2297 yeah"
       (lc/to-str { [ (->Concatter "hell"), (->Concatter "yeah") ] -17}))))

  (is (=
       "-17 hell +15 yeah"
       (lc/to-str { (->Concatter "hell") -17, (->Concatter "yeah") 15})))

  (is (=
       "-17\\ hell +15\\ yeah"
       (lc/to-latex { (->Concatter "hell") -17, (->Concatter "yeah") 15})))
       

(deftest lc-test
  (is (= {1 3, "something" 4, [1 2] 0.9}
         (lc/+ {1 1, "something" 2} {1 2, "something" 2, [1 2] 0.9} )))

  (is (= {"something" 8}
         (apply lc/+ (repeat 4 {"something" 2}))))

  (is (= {"something" 8}
         (lc/add-into {} '( ["something" 6] ["something" 2]))))

  (is (= {[1 2] 77}
         (lc/otimes {1 11} {2 7})))
  (is (= {}
         (lc/otimes {} {2 7})))

  (is (= {1 11, 3 33}
         (lc/filter odd? {1 11, 2 22, 3 33, 4 44})))

  (is (thrown? java.lang.AssertionError
               (lc/+ {"no" "way"} {"jo" "se"})))

  (is (= {} (lc/remove-zeros {"zero" 0})))

  (is (= {} (lc/* {} {})))

  (is (= {}
         (lc/multiply-using {(->Concatter "hell") 11} {} product)))

  (is (= {(->Concatter "hellyeah") 44}
         (lc/multiply-using {(->Concatter "hell") 11} {(->Concatter "yeah") 4} product)))

  (is (= {(->Concatter "hellyeah") 44}
         (lc/* {(->Concatter "hell") 11} {(->Concatter "yeah") 4})))

  (is (= {(->Concatter "hell") 44}
         (lc/* 4 {(->Concatter "hell") 11})))

  (is (= {(->Concatter "hellyeahyeah") 880}
         (lc/* 2 {(->Concatter "hell") 11} {(->Concatter "yeah") 4} {(->Concatter "yeah") 10})))

  (defn reverse-concat [a b]
    { (->Concatter (str (:content b) (:content a))) 1} )

  (is (= {(->Concatter "yeahhell") 44}
         (lc/multiply-using {(->Concatter "hell") 11} {(->Concatter "yeah") 4} reverse-concat)))

  (is (=
       (lc/apply-linear-function
         (fn [x] { (str x x) 1})
         {"one" 4/5})
         {"oneone" 4/5}))

  (is (=
       (lc/apply-linear-function
         (fn [x] { (str x x) 1})
         {})
         {}))

  (is (=
       { "ac" 3
         "ad" 4
         "bc" 6
         "bd" 8}
       (lc/apply-bilinear-function
         (fn [x y] { (str x y) 1 } )
         {"a" 1, "b" 2}
         {"c" 3, "d" 4})))
  )
