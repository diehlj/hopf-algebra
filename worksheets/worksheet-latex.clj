;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns exuberant-pine
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(require '[gorilla-repl.latex :as grl])

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(grl/latex-view "x^2")
;; @@
;; =>
;;; {"type":"latex","content":"x^2","value":"#gorilla_repl.latex.LatexView{:content \"x^2\"}"}
;; <=

;; @@
(grl/latex-view "x \\otimes y")
;; @@
;; =>
;;; {"type":"latex","content":"x \\otimes y","value":"#gorilla_repl.latex.LatexView{:content \"x \\\\otimes y\"}"}
;; <=

;; @@

;; @@
