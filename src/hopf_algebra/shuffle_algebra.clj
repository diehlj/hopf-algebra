(ns hopf-algebra.shuffle-algebra
  ^{:doc
    "Implements the shuffle Hopf algebra and its dual, the concatenation Hopf algebra.
     see e.g. Reutenauer - Free Lie Algebras"}
  (:require 
            [hopf-algebra.hopf-algebra :refer [product coproduct antipode to-latex to-str HopfAlgebra]]
            [clojure.math.combinatorics]
            [hopf-algebra.linear-combination :refer :all]))


(declare ->ShuffleWord)
(defn concat-shuffle-word [w1 w2]
  {(->ShuffleWord (concat (:content w1) (:content w2))) 1 })

(defn pick [ell-1-positions ell-1 ell-2]
  (loop [result []
         p-1 0
         p-2 0]
    (if (= (+ (count ell-1) (count ell-2))
           (+ p-1 p-2))
      result
      (let [i (+ p-1 p-2)
            pick-1? (some #{i} ell-1-positions)]
        (recur
          (conj result (if pick-1? (nth ell-1 p-1) (nth ell-2 p-2)))
          (if pick-1? (inc p-1) p-1)
          (if (not pick-1?) (inc p-2) p-2))))))

; XXX this is brute-force
(defn word-shuffle [w-1 w-2]
  (if (or (empty? w-1) (empty? w-2))
    (list (concat w-1 w-2))
    (let [words {1 w-1, 2 w-2},
          n-1 (count w-1)
          n-2 (count w-2)]
      (for [comb (clojure.math.combinatorics/combinations (range (+ n-1 n-2)) n-1)]
        (pick comb w-1 w-2)))))

(defn sw-shuffle [sw-1 sw-2]
  (let [w-1 (:content sw-1)
        w-2 (:content sw-2)]
    (lc-add-into {} (map (fn [w] [(->ShuffleWord w) 1]) (word-shuffle w-1 w-2)))))

(defn deconcatenation [w]
  (loop [n 0
         result {}]
    (if (> n (count w))
      result
      (recur (inc n)
             (lc-add result (lc-lift [(->ShuffleWord (take n w)) (->ShuffleWord (drop n w))]))))))

(defn shuffle-antipode [w]
  (lc-multiply
    (if (odd? (count w)) -1 1)
    (lc-lift (->ShuffleWord (reverse w)))))

(defrecord ShuffleWord
  ;;
  ; content = list/vector of stuff (usually integers)
  ;;
  [content])

;  Object (toString [w] (str "toString" (to-str w)))  ;- for (str ..)
;(defmethod print-method ShuffleWord [w ^java.io.Writer writer] ;- for (print ..)
;  (print-method (str "print-method: " (to-str w)) writer))

(extend-type ShuffleWord
  HopfAlgebra
  (product [a b] (sw-shuffle a b))
  (coproduct [a] (deconcatenation (:content a)))
  (antipode [a] (shuffle-antipode (:content a)))
  (counit [a] (if (empty? (:content a)) 1 0))
  (to-str [a] (if (empty? (:content a))
                   "e"
                   (apply str (:content a))))
  (to-latex [a] (to-str a))
  (gorilla-render [a] {:type :latex :content (to-latex a)
                                    :value (pr-str a)})
  )

(defn unit->ShuffleWord [c]
  {(->ShuffleWord []) c})

;;
; DUAL OF SHUFFLE ALGEBRA
;;
(declare ->ConcatWord)
(defn concat-coproduct [w]
  ;;
  ; this uses \Delta \tau \tau' = \Delta \tau \Delta \tau'
  ; to calculate the coproduct (deshuffle)
  ;;
  (if (= 0 (count w))
    {[(->ConcatWord []) (->ConcatWord [])] 1}
    (if (= 1 (count w))
      { [ (->ConcatWord w) (->ConcatWord [])] 1,
        [ (->ConcatWord []) (->ConcatWord w)] 1 }
      (lc-multiply (concat-coproduct (take 1 w))
                   (concat-coproduct (rest w))))))

(defn concat-antipode [w]
  (lc-multiply
    (if (odd? (count w)) -1 1)
    (lc-lift (->ConcatWord (reverse w))))) ;- the same as shuffle-antipode, apart from ->ConcatWord

(defrecord ConcatWord
  ;;
  ; content = list/vector of stuff (usually integers)
  ;;
  [content])

(extend-type ConcatWord
  HopfAlgebra
  (product [a b] (lc-lift (->ConcatWord (concat (:content a) (:content b)))))
  (coproduct [a] (concat-coproduct (:content a)))
  (antipode [a] (concat-antipode (:content a)))
  (counit [a] (if (empty? (:content a)) 1 0))
  (to-str [a] (if (empty? (:content a))
                   "e"
                   (apply str (:content a))))
  (to-latex [a] (to-str a))
  (gorilla-render [a] {:type :latex :content (to-latex a)
                                    :value (pr-str a)}))

(defn cw->sw [lc]
  (lc-apply-linear-function
    (fn [t]
      (letfn [(f [x] (->ShuffleWord (:content x)))]
      (if (tensor? t)
        { (map f t) 1}
        { (f t) 1})))
    lc))

(defn sw->cw [lc]
  (lc-apply-linear-function
    (fn [t]
      (letfn [(f [x] (->ConcatWord (:content x)))]
      (if (tensor? t)
        { (map f t) 1}
        { (f t) 1})))
    lc))

(defn inner-product-sw-cw [lc-sw lc-cw]
  (lc-inner-product lc-sw (cw->sw lc-cw)))
