(ns
  ^{:doc
    "A linear combination is of the form { e1 2.2, e2 9, .. }
     Often eX is a tensor which is of the form s1 or (s1,..,sn)
     where sX implements the HopfAlgebra interface.
    
     A linear combination \"plus\" is is of the form
     { e1 c1, e2 c2, .. }; where now cX itself are linear combinations."}
  hopf-algebra.linear-combination
  (:refer-clojure :exclude [+ - * filter])
  (:require [hopf-algebra.hopf-algebra :refer [product antipode counit HopfAlgebra] :as ha]))


;;
; I don't want spec dependence atm
;(def anything? (constantly true))
;(s/def ::lc (s/map-of anything? number?))
;(defn lc? [something]
;  (s/valid? ::lc something))
;;;

(defn lc?
  "A `linear combination` is of the form {object-1 22, object-2 19}."
  [something]
  (and
    (map? something)
    (every? (fn [ [k v] ] (number? v)) something)))

(defn tensor? [x]
  (or (vector? x) (seq? x)))

(defn- tensor-to-str [t]
  (if (tensor? t)
    (apply str (interpose " \u2297 " (map tensor-to-str t)))
    (ha/to-str t)))

(defn format-number
  "Helper function for to-str.
   Formats a number x depending on the position pos it has in a sum."
  [pos x]
  (if (= 0 pos)
    (if (= x 1)
      ""
      (if (= x -1)
        "-"
        (str x)))
    (if (= x 1)
      "+"
      (if (= x -1)
        "-"
        (if (< x 0)
          (str x)
          (str "+" x))))))

(defn to-str [lc]
  {:pre [(lc? lc)]}
  (->> lc
       (map-indexed (fn [pos [k v]] (str (format-number pos v) " " (tensor-to-str k))))
       (interpose " ")
       (apply str)))

(defn- tensor-to-latex [t]
  (if (tensor? t)
    (str (apply str (interpose " \\otimes " (map tensor-to-latex t))) )
    (ha/to-latex t)))

(defn- to-latex-helper [index a]
  (let [coefficient (format-number index (val a))]
    (if (empty? coefficient)
      (str (tensor-to-latex (key a)))
      (str coefficient "\\ " (tensor-to-latex (key a))))))

(defn to-latex [lc]
  {:pre [(lc? lc)]}
  (->> lc
       (map-indexed to-latex-helper)
       (interpose " ")
       (apply str)))

(defn remove-zeros
  "Remove all entries that have coefficient 0." 
  [lc]
  {:pre [(lc? lc)]}
  (into {} (remove #( or (= 0 (second %))
                         (= 0. (second %)) ) lc )))

(defn +
  "Add linear combinations."
  [& args]
  (if (empty? args)
    {}
    (remove-zeros (apply merge-with clojure.core/+ args))))

(defn- add-conj [coll x]
  (if (contains? coll (first x))
    (conj coll [(first x) (clojure.core/+ (second x) (coll (first x)))])
    (conj coll x)))

(defn add-into
  "Like `into` but for a linear combination (i.e. adding up instead of replacing)."
  [to from]
  (reduce add-conj to from))

(declare *)

(defn -
  "Substract two linear combinations."
  [a b]
  {:pre [(lc? a) (lc? b)]}
  (+ a (* -1 b)))

(defn- list-if-is-not [x]
  (if (tensor? x) x [x]))

(declare otimes)
(declare lift)

;;
; for some reason this is _much_ faster then the old implementation with
;    (into {} (map #( vector (first %) (clojure.core/* scalar (second %)) ) lc))))
;;
(defn- update-values [m f & args]
  (reduce (fn [result [k v]] (assoc result k (apply f v args))) {} m))
(defn- multiply-scalar [scalar lc]
  (if (= 0 scalar)
    {}
    (update-values lc (fn [v] (clojure.core/* scalar v)))))

(declare multiply-using)

(defn *
  "Multiply an arbitrary number of lc's.
   First argument can be a scalar."
  [ & args ]
  (if (empty? args)
    {}
    (if (= 1 (count args))
      (first args)
      (let [ a (first args)
             lcs (rest args) ]
        (let [r (reduce (fn [x y] (multiply-using x y product)) lcs)]
          (if (number? a)
            (multiply-scalar a r)
            (multiply-using a r product)))))))

(defn- tensor-otimes [t1 t2]
  "Tensor product of tensors:
    
    [1 2] 55 -> [1 2 55]
    [1 2] [55] -> [1 2 55]"
  (concat (list-if-is-not t1) (list-if-is-not t2)))

(defn lift [x]
  (if (lc? x)
    x
    {x 1}))

(defn- tensor-multiply-using
  "Componentwise product of tensors a,b using the function m."
  [a b m]
  {
   :pre [(or
           (and (not (tensor? a)) (not (tensor? b)))
           (and (tensor? a) (tensor? b) (= (count a) (count b))))
         ],
   ;:post [(lc? %)]
  }
  (if (not (tensor? a))
    (m a b)
    (loop [remaining-a a,
           remaining-b b,
           result {}]
      (if (empty? remaining-a)
        result
        (let [right (m (first remaining-a) (first remaining-b))]
          (recur
            (rest remaining-a)
            (rest remaining-b)
            (if (empty? result)
              right
              (otimes result right))))))))

(defn- multiply-using-helper
  [a b m]
  {:post [(lc? %)]}
  (* (clojure.core/* (val a) (val b)) (tensor-multiply-using (key a) (key b) m)))

; TODO lc-remove-zeros !
(defn multiply-using
  "Multiply linear combinations a,b using the function m.
   m must take two arguments (usually of type HopfAlgebra) and returns an lc."
  [a b m] ; XXX beware the difference to the signature of *
  ;{:pre [(lc? a) (lc? b)]}
  (if (or (empty? a) (empty? b))
    {}
    (apply merge-with clojure.core/+
          (for [ [k-1 v-1] a
                 [k-2 v-2] b ]
            (* (clojure.core/* v-1 v-2) (tensor-multiply-using k-1 k-2 m))))))

(defn- update-values [m f & args]
  (reduce (fn [result [k v]] (assoc result k (apply f v args))) {} m))

(declare apply-bilinear-function)
(defn otimes
  "Tensor product of linear combinations a,b."
  [a b]
  {:pre [(lc? a) (lc? b)]}
  (apply-bilinear-function
    (fn [x y] { (tensor-otimes x y) 1})
    a b))

(defn apply-linear-function [f lc]
  (if (empty? lc)
    lc
    (remove-zeros
      (apply merge-with clojure.core/+ (map (fn [ [k v] ] (* v (f k))) lc)))))

(defn coproduct
  "Apply the coproduct to an lc of HopfAlgebra elements."
  [lc]
  (apply-linear-function ha/coproduct lc))

(defn apply-bilinear-function
  "Apply a bilinear function f to lc-1 and lc-2.
  f must take two arguments (usually of type HopfAlgebra) and returns an lc.

    { a1 c1, a2 c2 }, {a3 c3} -> c1 * c3 * f(a1,a3) + c2 * c3 * f(a2,a3)"
  [f lc-1 lc-2]
  {:pre [(lc? lc-1) (lc? lc-2)]}
  (if (or (empty? lc-1) (empty? lc-2))
    {}
    (remove-zeros
      (apply merge-with clojure.core/+
             (for [ [k-1 v-1] lc-1
                    [k-2 v-2] lc-2 ]
               (* (clojure.core/* v-1 v-2) (f k-1 k-2)))))))

(defn filter
  "Filter the keys of an lc."
  [pred lc]
  {:pre [(lc? lc)]}
  (into {} (clojure.core/filter #( pred (key %) ) lc )))

(defn inner-product
  "Computes the inner product of lc-1, lc-2,
   assuming that its vectors (its keys) are orthonormal."
  [lc-1 lc-2]
  {:pre [(lc? lc-1) (lc? lc-2)]}
  (loop [result 0
         remaining (seq lc-1)]
    (if (empty? remaining)
      result
      (recur
        (clojure.core/+ result (clojure.core/*
                   (val (first remaining))
                   (get lc-2 (key (first remaining)) 0)))
        (rest remaining)))))


(defn tensor-antipode-otimes-id [t]
  (otimes (antipode (first t)) (lift (second t))))

(defn tensor-id-otimes-antipode [t]
  (otimes (lift (first t)) (antipode (second t))))

(defn tensor-m12 [t]
  (product (nth t 0) (nth t 1)))

(defn tensor-coproduct-otimes-id [t]
  (otimes (coproduct (first t)) (lift (second t))))

(defn tensor-id-otimes-coproduct [t]
  (otimes (lift (first t)) (coproduct (second t))))

(defn tensor-m1324 [t]
  "Product of the 1st with the 3rd and the 2nd with th 4th component of a 4-tensor."
  (otimes (product (nth t 0) (nth t 2)) (product (nth t 1) (nth t 3))))

(defn tensor-id-otimes-counit [t]
  (*
    (counit (nth t 1))
    (lift (nth t 0))))

;; these do not work:
;(defn- arg-count [f]
;  {:pre [(instance? clojure.lang.AFunction f)]}
;  (-> f class .getDeclaredMethods first .getParameterTypes alength))
;(defn tensor-functions
;  ([f1] (fn [t] (apply f1 t))) ; XXX this is a problem if t is a singelton
;  ([f1 f2] (fn [t] (otimes (apply f1 (take (arg-count f1) t)) ( apply f2 (drop (arg-count f1) t)))))
;  ([f1 f2 f3] (fn [t] (otimes (apply f1 (take (arg-count f1) t)) ( (tensor-functions f2 f3) (drop (arg-count f1) t))))))
;(def tensor-antipode-otimes-id (tensor-functions antipode lift))
;(def tensor-id-otimes-antipode (tensor-functions lift antipode))
;(def tensor-m12 (tensor-functions product))
;(defn tensor-flip [a b] (lift [b a])) ; XXX naming; tensor- vs  ..
;(def tensor-m1324
;  (comp (partial apply-linear-function (tensor-functions product product) )
;;
