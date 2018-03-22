(ns
  ^{:doc
    "A linear combination is of the form { e1 2.2, e2 9, .. }
     Often eX is a tensor which is of the form s1 or (s1,..,sn)
     where sX implements the HopfAlgebra interface.
    
     A linear combination \"plus\" is is of the form
     { e1 c1, e2 c2, .. }; where now cX itself are linear combinations."}
  hopf-algebra.linear-combination-plus
  (:refer-clojure :exclude [+ - * filter])
  (:require [hopf-algebra.hopf-algebra :refer [product coproduct antipode counit HopfAlgebra] :as ha]
            [hopf-algebra.linear-combination :as lc]))


;;
;(s/def ::lc-plus (s/map-of anything? ::lc))
;(defn lcp? [something]
;  (s/valid? ::lc-plus something))
;;

(defn lcp?
  "A `linear combination PLUS` is of the form {object-1 {c-1 22, 1 14/15}}"
  [something]
  (and (map? something)
       (every? (fn [ [k v] ] (lc/lc? v)) something)))


; one can put whatever one likes in the linear combination of coefficients appearing in a linear combination PLUS
; here is a default to use
(defrecord Coefficient [name options])

(extend-type Coefficient
  HopfAlgebra
  (to-str [a] (str (:name a)
                   (when (not (empty? (:options a)))
                     (str " " (:options a)))))
  (to-latex [a] (ha/to-str a))
  (gorilla-render [a] {:type :latex, :content (ha/to-latex a)
                                     :value (pr-str a)}))

(defn ->Coefficient
  ([name] (Coefficient. name nil))
  ([name options] (Coefficient. name options)))

(defn filter [f lc]
  {:pre [(lcp? lc)]}
  (into {} (clojure.core/filter #( f (key %) ) lc )))

(defn lift [x] ; XXX
  {:post [(lcp? %)]}
  { x {1 1} })

(defn remove-zeros
  "Remove all entries that have coefficient 0." 
  [lcp]
  {:pre [(lcp? lcp)]
   :post [(lcp? %)]}
  (into {} (clojure.core/filter #( not (= {} (second %)) )
                   (map (fn [[k v]] [k (lc/remove-zeros v)]) lcp ))))

(defn multiply-scalar
  [scalar lc]
  (if (= 0 scalar)
    {}
    (#'hopf-algebra.linear-combination/update-values lc (fn [v] (#'hopf-algebra.linear-combination/multiply-scalar scalar v)))))

(defn +
  "Add two linear combinations PLUS."
  ([] {})
  ([lcp] lcp)
  ([lcp1 lcp2]
    {:pre [(lcp? lcp1) (lcp? lcp2)] 
     :post [(lcp? %)]}
    (remove-zeros
      (merge-with
        (fn [x y] (lc/+ x y))
        lcp1 lcp2))))


(defn- apply-linear-function-helper [lc-val lc]
  ;;
  ;| lc-val = {"C1" 55}    lc = {object-1 10, object-2 100}
  ;| => {object-1 {"C1" 550}, object-2 {"C1" 5500}}
  ;;
  (into {} (map (fn [ [k v] ] [k (lc/* v lc-val)]) lc)))
  

(defn apply-linear-function
  "
    f(x) = 7 xx
    lcp = {x {c-1 1}, y {c-2 3}}
    => {xx {c-1 7}, yy {c-2 21}}"
  [f lcp]
  {:pre [(lcp? lcp)]
   :post [(lcp? %)]}
  (remove-zeros
    (reduce 
      (fn [result nexxt] (+ result
                                      (apply-linear-function-helper (val nexxt) (f (key nexxt)))))
      {}
      lcp)))

(defn- apply-linear-function'-helper [lc-val lc]
  (into {} (map (fn [ [k v] ] [k (lc/* lc-val v)]) lc)))

(defn apply-linear-function'
  " f(x) = {x {'c-1' 7}}
    lc = {x-1 2}
    => { x-1 {'c-1' 14} }"
  [f lc]
  (remove-zeros
    (reduce 
      (fn [result nexxt] (+ result
                                      (apply-linear-function'-helper (val nexxt) (f (key nexxt)))))
      {}
      lc)))

(defn- multiply-two
  [a b]
  (if (or (empty? a) (empty? b))
    {}
    (apply merge-with lc/+
          (for [ [k-1 v-1] a
                 [k-2 v-2] b ]
            (let [coefficient (lc/* v-1 v-2)]
              (apply-linear-function'
                (fn [x] {x coefficient})
                (#'hopf-algebra.linear-combination/tensor-multiply-using k-1 k-2 product)))))))

(defn *
  "Multiply an arbitrary number of lcp's.
   First argument can be a scalar."
  [ & args ]
  (if (empty? args)
    {}
    (if (= 1 (count args))
      (first args)
      (let [ a (first args)
             lcs (rest args) ]
        (let [r (reduce (fn [x y] (multiply-two x y)) lcs)]
          (if (number? a)
            (multiply-scalar a r)
            (multiply-two a r)))))))

(defn- to-latex-helper [index kv]
  (let [coefficient (str "+ \\left( " (lc/to-str (val kv)) "\\right)")]
      (str coefficient "\\ " (#'hopf-algebra.linear-combination/tensor-to-latex (key kv)))))

(defn to-latex [lcp]
  {:pre [(lcp? lcp)]}
  (->> lcp
       (map-indexed to-latex-helper)
       (interpose " " )
       (apply str)))

(defn- to-str-helper [index kv]
  (let [coefficient (str "+ ( " (lc/to-str (val kv)) " )")] ; TODO first one should not get a "+"
    (str coefficient " " (#'hopf-algebra.linear-combination/tensor-to-str (key kv)))))

(defn to-str
  [lcp]
  {:pre [(lcp? lcp)]}
  (->> lcp
       (map-indexed to-str-helper)
       (interpose " " )
       (apply str)))

(defn lc-lc-plus-inner-product
  "Computes the inner product of lc, lcp,
   assuming that its vectors (its keys) are orthonormal."
  [lc lcp]
  {:pre [(lc/lc? lc) (lcp? lcp)]}
  (loop [result {}
         remaining (seq lc)]
    (if (empty? remaining)
      result
      (recur
        (lc/+ result
                (lc/*
                   (val (first remaining))
                   (get lcp (key (first remaining)) 0)))
        (rest remaining)))))

(defn lc->lcp
  "Converts a linear combination to a linear combination PLUS, i.e.
    
    {object-1 22}, coefficient => {object-1 {coefficient 22}}"
  [lc coefficient]
  {:pre [(lc/lc? lc)]
   :post [(lcp? %)]}
  (into {} (map (fn [ [k v] ] [ k { coefficient v } ]) lc)))
