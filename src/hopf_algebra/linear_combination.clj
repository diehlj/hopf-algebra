(ns
  ^{:doc
    "A linear combination is of the form { e1 2.2, e2 9, .. }
     Often eX is a tensor which is of the form s1 or (s1,..,sn)
     where sX implements the HopfAlgebra interface.
    
     A linear combination \"plus\" is is of the form
     { e1 c1, e2 c2, .. }; where now cX itself are linear combinations."}
  hopf-algebra.linear-combination
  (:require [hopf-algebra.hopf-algebra :refer [product coproduct antipode counit to-str to-latex]]))



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
    (to-str t)))

(defn lc-format-number
  "Helper function for lc-to-str.
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

(defn lc-to-str [lc]
  {:pre [(lc? lc)]}
  (->> lc
       (map-indexed (fn [pos [k v]] (str (lc-format-number pos v) " " (tensor-to-str k))))
       (interpose " ")
       (apply str)))

(defn- tensor-to-latex [t]
  (if (tensor? t)
    (str (apply str (interpose " \\otimes " (map tensor-to-latex t))) )
    (to-latex t)))

(defn- lc-to-latex-helper [index a]
  (let [coefficient (lc-format-number index (val a))]
    (if (empty? coefficient)
      (str (tensor-to-latex (key a)))
      (str coefficient "\\ " (tensor-to-latex (key a))))))

(defn lc-to-latex [lc]
  {:pre [(lc? lc)]}
  (->> lc
       (map-indexed lc-to-latex-helper)
       (interpose " ")
       (apply str)))

(defn lc-remove-zeros
  "Remove all entries that have coefficient 0." 
  [lc]
  {:pre [(lc? lc)]}
  (into {} (remove #( or (= 0 (second %))
                         (= 0. (second %)) ) lc )))

(defn lc-add
  "Add linear combinations."
  [& args]
  (if (empty? args)
    {}
    (lc-remove-zeros (apply merge-with + args))))

(defn- lc-add-conj [coll x]
  (if (contains? coll (first x))
    (conj coll [(first x) (+ (second x) (coll (first x)))])
    (conj coll x)))

(defn lc-add-into
  "Like `into` but for a linear combination (i.e. adding up instead of replacing)."
  [to from]
  (reduce lc-add-conj to from))

(declare lc-multiply)

(defn lc-substract
  "Substract two linear combinations."
  [a b]
  {:pre [(lc? a) (lc? b)]}
  (lc-add a (lc-multiply -1 b)))

(defn- list-if-is-not [x]
  (if (tensor? x) x [x]))

(declare lc-otimes)
(declare lc-lift)

;;
; for some reason this is _much_ faster then the old implementation with
;    (into {} (map #( vector (first %) (* scalar (second %)) ) lc))))
;;
(defn- update-values [m f & args]
  (reduce (fn [result [k v]] (assoc result k (apply f v args))) {} m))
(defn- lc-multiply-scalar [scalar lc]
  (if (= 0 scalar)
    {}
    (update-values lc (fn [v] (* scalar v)))))

(declare lc-multiply-using)

(defn lc-multiply ; XXX naming .. vs lc-product
  "Multiply an arbitrary number of lc's.
   First argument can be a scalar."
  [ & args ]
  (if (empty? args)
    {}
    (if (= 1 (count args))
      (first args)
      (let [ a (first args)
             lcs (rest args) ]
        (let [r (reduce (fn [x y] (lc-multiply-using x y product)) lcs)]
          (if (number? a)
            (lc-multiply-scalar a r)
            (lc-multiply-using a r product)))))))

(defn- tensor-otimes [t1 t2]
  "Tensor product of tensors:
    
    [1 2] 55 -> [1 2 55]
    [1 2] [55] -> [1 2 55]"
  (concat (list-if-is-not t1) (list-if-is-not t2)))

(defn lc-lift [x]
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
              (lc-otimes result right))))))))

(defn- lc-multiply-using-helper
  [a b m]
  {:post [(lc? %)]}
  (lc-multiply (* (val a) (val b)) (tensor-multiply-using (key a) (key b) m)))

(defn lc-multiply-using
  "Multiply linear combinations a,b using the function m.
   m must take two arguments (usually of type HopfAlgebra) and returns an lc."
  [a b m] ; XXX beware the difference to the signature of lc-multiply
  ;{:pre [(lc? a) (lc? b)]}
  (if (or (empty? a) (empty? b))
    {}
    (apply merge-with +
          (for [ [k-1 v-1] a
                 [k-2 v-2] b ]
            (lc-multiply (* v-1 v-2) (tensor-multiply-using k-1 k-2 m))))))

(defn- update-values [m f & args]
  (reduce (fn [result [k v]] (assoc result k (apply f v args))) {} m))

(declare lc-apply-bilinear-function)
(defn lc-otimes
  "Tensor product of linear combinations a,b."
  [a b]
  {:pre [(lc? a) (lc? b)]}
  (lc-apply-bilinear-function
    (fn [x y] { (tensor-otimes x y) 1})
    a b))

(defn lc-apply-linear-function [f lc]
  (if (empty? lc)
    lc
    (lc-remove-zeros
      (apply merge-with + (map (fn [ [k v] ] (lc-multiply v (f k))) lc)))))

(defn lc-coproduct
  "Apply the coproduct to an lc of HopfAlgebra elements."
  [lc]
  (lc-apply-linear-function coproduct lc))

(defn lc-apply-bilinear-function
  "Apply a bilinear function f to lc-1 and lc-2.
  f must take two arguments (usually of type HopfAlgebra) and returns an lc.

    { a1 c1, a2 c2 }, {a3 c3} -> c1 * c3 * f(a1,a3) + c2 * c3 * f(a2,a3)"
  [f lc-1 lc-2]
  {:pre [(lc? lc-1) (lc? lc-2)]}
  (if (or (empty? lc-1) (empty? lc-2))
    {}
    (lc-remove-zeros
      (apply merge-with +
             (for [ [k-1 v-1] lc-1
                    [k-2 v-2] lc-2 ]
               (lc-multiply (* v-1 v-2) (f k-1 k-2)))))))

(defn lc-filter
  "Filter the keys of an lc."
  [pred lc]
  {:pre [(lc? lc)]}
  (into {} (filter #( pred (key %) ) lc )))

(defn lc-inner-product
  "Computes the inner product of lc-1, lc-2,
   assuming that its vectors (its keys) are orthonormal."
  [lc-1 lc-2]
  {:pre [(lc? lc-1) (lc? lc-2)]}
  (loop [result 0
         remaining (seq lc-1)]
    (if (empty? remaining)
      result
      (recur
        (+ result (*
                   (val (first remaining))
                   (get lc-2 (key (first remaining)) 0)))
        (rest remaining)))))


;;
;(s/def ::lc-plus (s/map-of anything? ::lc))
;(defn lcp? [something]
;  (s/valid? ::lc-plus something))
;;

(defn lcp?
  "A `linear combination PLUS` is of the form {object-1 {c-1 22, 1 14/15}}"
  [something]
  (and (map? something)
       (every? (fn [ [k v] ] (lc? v)) something)))

(defn lc-plus-filter [f lc]
  {:pre [(lcp? lc)]}
  (into {} (filter #( f (key %) ) lc )))

(defn lc-plus-lift [x]
  {:post [(lcp? %)]}
  { x {1 1} })

(defn lc-plus-remove-zeros
  "Remove all entries that have coefficient 0." 
  [lcp]
  {:pre [(lcp? lcp)]
   :post [(lcp? %)]}
  (into {} (filter #( not (= {} (second %)) )
                   (map (fn [[k v]] [k (lc-remove-zeros v)]) lcp ))))

(defn lc-plus-multiply-scalar
  [scalar lc]
  (if (= 0 scalar)
    {}
    (update-values lc (fn [v] (lc-multiply-scalar scalar v)))))

(defn lc-plus-add
  "Add two linear combinations PLUS."
  ([] {})
  ([lcp] lcp)
  ([lcp1 lcp2]
    {:pre [(lcp? lcp1) (lcp? lcp2)] 
     :post [(lcp? %)]}
    (lc-plus-remove-zeros
      (merge-with
        (fn [x y] (lc-add x y))
        lcp1 lcp2))))

(defn- lc-plus-apply-linear-function-multiply [lc-val lc] ; XXX name
  ;;
  ;| lc-val = {"C1" 55}    lc = {object-1 10, object-2 100}
  ;| => {object-1 {"C1" 550}, object-2 {"C1" 5500}}
  ;;
  (into {} (map (fn [ [k v] ] [k (lc-multiply v lc-val)]) lc)))
  

(defn lc-plus-apply-linear-function
  "
    f(x) = 7 xx
    lcp = {x {c-1 1}, y {c-2 3}}
    => {xx {c-1 7}, yy {c-2 21}}"
  [f lcp]
  {:pre [(lcp? lcp)]
   :post [(lcp? %)]}
  (lc-plus-remove-zeros
    (reduce 
      (fn [result nexxt] (lc-plus-add result
                                      (lc-plus-apply-linear-function-multiply (val nexxt) (f (key nexxt)))))
      {}
      lcp)))

(defn lc->lcp
  "Converts a linear combination to a linear combination PLUS, i.e.
    
    {object-1 22}, coefficient => {object-1 {coefficient 22}}"
  [lc coefficient]
  {:pre [(lc? lc)]
   :post [(lcp? %)]}
  (into {} (map (fn [ [k v] ] [ k { coefficient v } ]) lc)))

(defn- lc-plus-to-latex-helper [index kv]
  (let [coefficient (str "+ \\left( " (lc-to-str (val kv)) "\\right)")]
      (str coefficient "\\ " (tensor-to-latex (key kv)))))

(defn lc-plus-to-latex [lcp]
  {:pre [(lcp? lcp)]}
  (->> lcp
       (map-indexed lc-plus-to-latex-helper)
       (interpose " " )
       (apply str)))

(defn- lc-plus-to-str-helper [index kv]
  (let [coefficient (str "+ ( " (lc-to-str (val kv)) " )")]
    (str coefficient "\\ " (tensor-to-str (key kv)))))

(defn lc-plus-to-str
  [lcp]
  {:pre [(lcp? lcp)]}
  (->> lcp
       (map-indexed lc-plus-to-str-helper)
       (interpose " " )
       (apply str)))





(defn tensor-antipode-otimes-id [t]
  (lc-otimes (antipode (first t)) (lc-lift (second t))))

(defn tensor-id-otimes-antipode [t]
  (lc-otimes (lc-lift (first t)) (antipode (second t))))

(defn tensor-m12 [t]
  (product (nth t 0) (nth t 1)))

(defn tensor-coproduct-otimes-id [t]
  (lc-otimes (coproduct (first t)) (lc-lift (second t))))

(defn tensor-id-otimes-coproduct [t]
  (lc-otimes (lc-lift (first t)) (coproduct (second t))))

(defn tensor-m1324 [t]
  "Product of the 1st with the 3rd and the 2nd with th 4th component of a 4-tensor."
  (lc-otimes (product (nth t 0) (nth t 2)) (product (nth t 1) (nth t 3))))

(defn tensor-id-otimes-counit [t]
  (lc-multiply
    (counit (nth t 1))
    (lc-lift (nth t 0))))

;; these do not work:
;(defn- arg-count [f]
;  {:pre [(instance? clojure.lang.AFunction f)]}
;  (-> f class .getDeclaredMethods first .getParameterTypes alength))
;(defn tensor-functions
;  ([f1] (fn [t] (apply f1 t))) ; XXX this is a problem if t is a singelton
;  ([f1 f2] (fn [t] (lc-otimes (apply f1 (take (arg-count f1) t)) ( apply f2 (drop (arg-count f1) t)))))
;  ([f1 f2 f3] (fn [t] (lc-otimes (apply f1 (take (arg-count f1) t)) ( (tensor-functions f2 f3) (drop (arg-count f1) t))))))
;(def tensor-antipode-otimes-id (tensor-functions antipode lc-lift))
;(def tensor-id-otimes-antipode (tensor-functions lc-lift antipode))
;(def tensor-m12 (tensor-functions product))
;(defn tensor-flip [a b] (lc-lift [b a])) ; XXX naming; tensor- vs lc- ..
;(def tensor-m1324
;  (comp (partial lc-apply-linear-function (tensor-functions product product) )
;;
