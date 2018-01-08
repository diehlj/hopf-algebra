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
(require '[gorilla-repl.html :as grh])
gorilla-repl
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(grl/latex-view "x^2")
(grh/)
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
(require '[gorilla-renderable.core :as render])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defrecord Listy [data])

(extend-type Listy
  render/Renderable
;  (render [self] {:type :html, :content "<b>hell yeah</b>"}))
  (render [self] {:type :list-like, :open "", :close "", :seperator ",", :items (:data self)}));[{:type :html, :content "<b>hell</b>"}]}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(->Listy [(render/render (grl/latex-view "x^2")), (render/render (grl/latex-view "x^3"))])
(println (grl/latex-view "x^2"))
(grl/latex-view "x^2")
;; @@
;; ->
;;; #gorilla_repl.latex.LatexView{:content x^2}
;;; 
;; <-
;; =>
;;; {"type":"latex","content":"x^2","value":"#gorilla_repl.latex.LatexView{:content \"x^2\"}"}
;; <=

;; @@

;; @@
