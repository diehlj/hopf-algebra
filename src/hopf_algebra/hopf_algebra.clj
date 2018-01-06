(ns hopf-algebra.hopf-algebra
  ^{:doc
    "The HopfAlgebra protocol models a Hopf algebra https://en.wikipedia.org/wiki/Hopf_algebra.
     Since a Hopf algebra is a vector space, this is tightly connected with linear_combination.clj."}
  (:require [clojure.spec :as s]))
  
(defprotocol HopfAlgebra
  (product [a b]) ;- returns linear combination
  (coproduct [a]) ;- returns linear combination
  (antipode [a]) ;- returns a linear combination
  ;(unit [r]) ; does not make sense here, since unit: \R \to H; so, if needed, this is implemented
  ;           ; by convenction as unit->ImplementationHopfAlgebra
  (counit [a]) ;- H \to \R
  (to-str [a]) ;- returns string
  (to-latex [a]) ;- returns string
  (gorilla-render [a])) ;- for rendering in the gorilla-repl
                        ;- sometimes, to-latex will do the job; sometimes not

; https://groups.google.com/forum/#!topic/clojure/f068WTgakpk
;(defn product [a b]
;  (-product a b))
