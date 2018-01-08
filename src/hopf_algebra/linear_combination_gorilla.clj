(ns hopf-algebra.linear-combination-gorilla
  "Methods to render linear combinations and HopfAlgebra elements in the gorilla repl."
  (:require [clojure.data.codec.base64 :as b64]
            [clojure.string :as str]
            [hopf-algebra.linear-combination :as lc]
            [hopf-algebra.hopf-algebra :refer [to-str gorilla-render HopfAlgebra]]
            [gorilla-renderable.core :as render]
            ))

(defn- tensor-render [t]
  {
   :type :list-like
   :open "",
   :close "",
   :separator " ",
   :items (interpose {:type :html, :content "\u2297"} (map gorilla-render t))
  })

(defn- lc-render-helper [index a]
  {
  :type :list-like,
  :open "",
  :close "",
  :separator " ",
  :items [ {:type :html, :content (lc/lc-format-number index (val a))}
            (if (lc/tensor? (key a))
              (tensor-render (key a))
              (gorilla-render (key a))) ]
  :value (pr-str a)})

(defn- lc-render [lc]
  {
  :type :list-like,
  :open "",
  :close "",
  :separator " ",
  :items (map-indexed lc-render-helper (seq lc)),
  :value (pr-str lc)})


(defrecord LcView [content])

(defn lc-view
  "View a linear combination in a Gorilla REPL."
  [content] (LcView. content))


(extend-type LcView
  render/Renderable
  (render [self] (lc-render (:content self))))




(defn- lc-plus-render-helper [index a]
  {
  :type :list-like,
  :open "",
  :close "",
  :separator " ",
  :items [{:type :latex, :content (if (= 0 index) "(" "+ (") }
          (lc-render (val a))
          {:type :latex, :content ")"}
          (if (lc/tensor? (key a))
            (tensor-render (key a))
            (gorilla-render (key a))) ]
  :value (pr-str a)})

(defn- lc-plus-render [lcp]
  {
  :type :list-like,
  :open "",
  :close "",
  :separator " ",
  :items (map-indexed lc-plus-render-helper (seq lcp)),
  :value (pr-str lcp)})

(defrecord LcPlusView [content])

(defn lc-plus-view
  "View a linear combination PLUS in a Gorilla REPL."
  [content] (LcPlusView. content))


(extend-type LcPlusView
  render/Renderable
  (render [self] (lc-plus-render (:content self))))



(defrecord HopfAlgebraView [content])

(defn hopf-algebra-view
  "View a HopfAlgebra element in a Gorilla REPL."
  [content] (HopfAlgebraView. content))

(extend-type HopfAlgebraView
  render/Renderable
  (render [self] (gorilla-render (:content self))))



;; ;;;; This file is part of gorilla-repl. Copyright (C) 2014-, Jony Hudson.
;; ;;;;
;; ;;;; gorilla-repl is licenced to you under the MIT licence. See the file LICENCE.txt for full details.
;; 
;; ;(ns gorilla-repl.latex
;;   ;(:require [gorilla-renderable.core :as render]))
;; 
;; (defrecord EnhancedLatexView [content])
;; 
;; (defn enhanced-latex-view [content] (EnhancedLatexView. content))
;; 
;; (def latex-preamble
;;   "\\newcommand{\\s}[1]{{\\color{black}{#1}}}
;;    \\newcommand{\\I}{\\mathcal{I}}
;;    \\newcommand{\\PI}{\\mathbf{\\Pi}}")
;; 
;; (extend-type EnhancedLatexView
;;   render/Renderable
;;   (render [self]
;;     {:type :latex :content (str latex-preamble (:content self)) :value (pr-str self)}))
