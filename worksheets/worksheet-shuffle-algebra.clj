;; gorilla-repl.fileformat = 1

;; **
;;; # The Shuffle and Concatentation Hopf algebras
;; **

;; @@
(ns worksheet-shuffle-algebra
  (:require [gorilla-plot.core :as plot]
            [hopf-algebra.linear-combination :as lc]
            [hopf-algebra.linear-combination-gorilla :as lcg]
            [hopf-algebra.shuffle-algebra :as sa]
            [hopf-algebra.hopf-algebra :refer [product coproduct antipode counit to-str to-latex]]
            ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Shuffle product
;; **

;; @@
(def sw-1 (sa/->ShuffleWord [1 2 3]))
(def sw-2 (sa/->ShuffleWord [4 5]))

(lcg/hopf-algebra-view sw-1)
(lcg/hopf-algebra-view sw-2)

"Their shuffle product is:"
(lcg/lc-view (product sw-1 sw-2))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":""},{"type":"latex","content":"12453"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"14523"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"14253"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"12435"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"41523"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"45123"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"12345"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"14235"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"41235"}],"value":""},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"+"},{"type":"latex","content":"41253"}],"value":""}],"value":""}
;; <=

;; **
;;; Concatentation product
;; **

;; @@
(def cw-1 (sa/->ConcatWord [1 2 3]))
(def cw-2 (sa/->ConcatWord [4 5]))

(lcg/hopf-algebra-view cw-1)
(lcg/hopf-algebra-view cw-2)


"Their concatentation product is:"
(lcg/lc-view (product cw-1 cw-2))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":""},{"type":"latex","content":"12345"}],"value":""}],"value":""}
;; <=
