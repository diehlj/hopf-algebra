(ns hopf-algebra.shuffle-algebra-test
  (:require [clojure.test :refer :all]
            [hopf-algebra.hopf-algebra :refer [product coproduct antipode to-str HopfAlgebra]]
            [hopf-algebra.shuffle-algebra :refer :all]
            [hopf-algebra.linear-combination :refer :all]))

(deftest misc []
    (is (= [1 2 66 3] (#'hopf-algebra.shuffle-algebra/pick [0 1 3] [1 2 3] [66])))
    (is (= [66 1 2] (#'hopf-algebra.shuffle-algebra/pick [1 2] [1 2] [66])))
    )

(deftest shuffle-test
  ; product
  (is (=
       {(->ShuffleWord [1 2 3]) 1, (->ShuffleWord [1 3 2]) 1, (->ShuffleWord [3 1 2]) 1}
       (product (->ShuffleWord [1 2]) (->ShuffleWord [3]))))

  (is (=
       {(->ShuffleWord [2 1 2 1]) 2, (->ShuffleWord [2 2 1 1]) 4}
       (product (->ShuffleWord [2 1]) (->ShuffleWord [2 1]))))

  (is (= {(->ShuffleWord [1 2 3]) 1, (->ShuffleWord [1 3 2]) 1, (->ShuffleWord [3 1 2]) 1}
         (lc-multiply (lc-lift (->ShuffleWord [1 2]))
                      (lc-lift (->ShuffleWord [3])))))

  ;- condition on product vs coproduct: \Delta( \tau \tau' ) = \Delta(\tau) \Delta(\tau')
  (is (=
       (lc-apply-linear-function coproduct (product (->ShuffleWord [11 77]) (->ShuffleWord [22])))
       (lc-apply-linear-function tensor-m1324 (lc-otimes (coproduct (->ShuffleWord [11 77])) (coproduct (->ShuffleWord [22]))))))
  ;- condition on antipode: \Nabla (A \otimes id) \Delta = \eta \vareps
  (is (= {(->ShuffleWord []) 1}
         (lc-apply-linear-function tensor-m12 (lc-apply-linear-function tensor-antipode-otimes-id (coproduct (->ShuffleWord []))))))
  (is (= {}
         (lc-apply-linear-function tensor-m12 (lc-apply-linear-function tensor-antipode-otimes-id (coproduct (->ShuffleWord [1]))))))
  (is (= {}
         (lc-apply-linear-function tensor-m12 (lc-apply-linear-function tensor-antipode-otimes-id (coproduct (->ShuffleWord [1 2]))))))

  (let [tau {(->ShuffleWord []) 17, (->ShuffleWord [3]) -5, (->ShuffleWord [1 3 2]) 22}]
    ;- unit
    (is (=
         (lc-multiply 17 tau)
         (lc-multiply (unit->ShuffleWord 17) tau)))
    ;- counit
    (is (=
         (lc-apply-linear-function
           tensor-id-otimes-counit
           (lc-coproduct tau))
         tau)))
  )

(deftest concat-test
  (is (= {(->ConcatWord [1 2 3]) 1} (product (->ConcatWord [1 2]) (->ConcatWord [3]))))
  (is (=  
        { [(->ConcatWord [4]) (->ConcatWord []) ] 1,
          [(->ConcatWord []) (->ConcatWord [4]) ] 1 },
        (coproduct (->ConcatWord [4]))))
  (is (=
       (lc-apply-linear-function coproduct (product (->ConcatWord [1]) (->ConcatWord [2 3])))
       (lc-apply-linear-function tensor-m1324 (lc-otimes (coproduct (->ConcatWord [1])) (coproduct (->ConcatWord [2 3]))))))
  (is (= {(->ConcatWord []) 1}
         (lc-apply-linear-function tensor-m12 (lc-apply-linear-function tensor-antipode-otimes-id (coproduct (->ConcatWord []))))))
  (is (= {}
         (lc-apply-linear-function tensor-m12 (lc-apply-linear-function tensor-antipode-otimes-id (coproduct (->ConcatWord [4]))))))

  (is (= {}
         (lc-apply-linear-function tensor-m12 (lc-apply-linear-function tensor-antipode-otimes-id (coproduct (->ConcatWord [4 5 6]))))))

 ; (let
 ;   [x (->ShuffleWord [2 1])
 ;    y (->ShuffleWord [2 1])
 ;    a (->ConcatWord [2])]
 ;   (println (lc-otimes (lc-lift x)
 ;                       (lc-lift y)))
 ;   (println (coproduct a))
 ;   (println (product x y))
 ;   (println (lc-lift a))
 ;   )

  ;- duality
  (let [words [ [1] [2] [1 2] [2 1] ]]
    (doseq [x (map ->ShuffleWord words)
            y (map ->ShuffleWord words)
            a (map ->ConcatWord words)
            b (map ->ConcatWord words)]
      (is (=
           (inner-product-sw-cw
             (coproduct x)
             (lc-otimes (lc-lift a)
                        (lc-lift b)))
           (inner-product-sw-cw
             (lc-lift x)
             (product a b))))
      (is (=
           (inner-product-sw-cw
             (lc-otimes (lc-lift x)
                        (lc-lift y))
             (coproduct a))
           (inner-product-sw-cw
             (product x y)
             (lc-lift a))))
      )
    )
)
