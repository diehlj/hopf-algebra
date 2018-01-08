;; gorilla-repl.fileformat = 1

;; **
;;; This worksheet demonstrates how a linear combination PLUS can be used for simple algebraic calculations.
;; **

;; @@
(ns worksheet-lc-plus-2
  (:require [gorilla-plot.core :as plot]
            ;[hopf-algebra.linear-combination :as lc]
            ;[hopf-algebra.linear-combination-gorilla :as lcg]
            [hopf-algebra.shuffle-algebra :as sa]
            [hopf-algebra.hopf-algebra :refer [product coproduct antipode counit to-str to-latex]]
            ))
(require '[hopf-algebra.linear-combination :as lc] :reload)
(require '[hopf-algebra.linear-combination-gorilla :as lcg] :reload)
(require '[hopf-algebra.shuffle-algebra :as sa] :reload)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We check how to express in dimension 2 on level 3, the coefficients of the Lyndon basis in terms of "the" Hall basis. (On this level one could of course to this by hand ..)
;; **

;; @@
(defn bracket [sw-1 sw-2]
  {(sa/->ConcatWord (concat (:content sw-1) (:content sw-2))) 1
   (sa/->ConcatWord (concat (:content sw-2) (:content sw-1))) -1})

(defn b [lc-1 lc-2]
  (lc/lc-apply-bilinear-function bracket lc-1 lc-2))

(def x1 {(sa/->ConcatWord [1]) 1})
(def x2 {(sa/->ConcatWord [2]) 1})

; "the" Hall basis; see https://coropa.sourceforge.io/
(def third-level-in-terms-of-hall-basis
   (lc/lc-plus-add
     (lc/lc->lcp (b x1 (b x1 x2)) (lc/->Coefficient "C1")) ; [1,[1,2]]
	 (lc/lc->lcp (b x2 (b x1 x2)) (lc/->Coefficient "C2")) ; [2,[1,2]]
	     ))

(lcg/lc-plus-view third-level-in-terms-of-coropa-basis)

; the Lyndon basis on the third level is
(def L31 (b (b x1 x2) x2)) ; [[1,2],2]
(def L32 (b x1 (b x1 x2))) ; [1,[1,2]]

; their dual elements are
(def L31* {(sa/->ShuffleWord [1 2 2]) 1})
(def L32* {(sa/->ShuffleWord [1 1 2]) 1})

; sanity check
(assert (= 1 (lc/lc-inner-product (sa/sw->cw L31*) L31)))
(assert (= 1 (lc/lc-inner-product (sa/sw->cw L32*) L32)))

"the coefficients of the Lyndon basis in terms of coefficients of the Hall basis:"
(lcg/lc-view (lc/lc-lc-plus-inner-product (sa/sw->cw L31*) third-level-in-terms-of-hall-basis))
(lcg/lc-view (lc/lc-lc-plus-inner-product (sa/sw->cw L32*) third-level-in-terms-of-hall-basis))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":""},{"type":"latex","content":"C1","value":"#hopf_algebra.linear_combination.Coefficient{:name \"C1\", :options nil}"}],"value":"[#hopf_algebra.linear_combination.Coefficient{:name \"C1\", :options nil} 1]"}],"value":"{#hopf_algebra.linear_combination.Coefficient{:name \"C1\", :options nil} 1}"}
;; <=

;; @@

;; @@
