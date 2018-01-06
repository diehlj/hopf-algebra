# hopf-algebra

A small library implementing [Hopf Algebras](https://en.wikipedia.org/wiki/Hopf_algebra) in clojure.
Its core is a class implementing a linear combination of "stuff".
Such a linear combination is stored as a map, mapping "stuff" to its coefficient.
That is {"a" 5, "b" 17} stands for the linear combination 5 * "a" + 17 * "b".

```clojure
(require '[hopf-algebra.linear-combination :as lc])

; vetor space operations; i.e addition / multiplication
(let [lc-1 { "something" 5 }
      lc-2 { "something" 3,
             "something else" 2}]]
  (lc/add (lc/multiply 10 lc-1) lc-2)) ; -> { "something" 53, "something else" 2}))
```

If "stuff" implements the HopfAlgebra protocol, we can do more:

```clojure
; algebra operations
(defrecord Concatter [content])
(extend-type Concatter
  HopfAlgebra
  (product [a b] { (->Concatter (str (:content a) (:content b))) 1 } ))

(let [lc-1 { "something" 5 }
      lc-2 { "else" 2}]]
  (lc/lc-multiply lc-1 lc-2)) ; -> { "somethingelse" 10 }
```

It comes with two sample implementations of Hopf algebras;
the shuffle Hopf algebra and, its dual, the concatenation Hopf algebra.

```clojure
(require '[hopf-algebra.shuffle-algebra :as sa])
(require [hopf-algebra.hopf-algebra :refer [product coproduct antipode]]
(let [sw-1 (sa/->ShuffleWord [1 2]),
      cw-1 (sa/->ConcatWord [1 2])]
  (product sw-1 sw-1) ; the shuffle of [1 2] and [1 2] as a linear combination:
                      ; {(->ShuffleWord [2 1 2 1]) 2, (->ShuffleWord [2 2 1 1]) 4}
  (lc/lc-multiply {sw-1 1} {sw-1 1}) ; gives the same result
  (product cw-1 cw-1) ; {(->ConcatWord [1 2 1 2]) 1}

  (coproduct sw-1)  ; the coproduct is de-concatenation
                    ; { [(->ShuffleWord [1 2]) (->ShuffleWord [])] 1,
                        [(->ShuffleWord [1]) (->ShuffleWord [2])] 1,
                        [(->ShuffleWord []) (->ShuffleWord [1 2])] 1}
                    ; note that tensors are stored as vectors
  (antipode sw-1)    ; {(->ShuffleWord [2 1]) 1}

  ; we verify one of the axioms for an antipode
  (= {(->ShuffleWord []) 1}
     (lc/lc-apply-linear-function lc/tensor-m12 (lc/lc-apply-linear-function lc/tensor-antipode-otimes-id (coproduct (->ShuffleWord [])))))
  (= {}
     (lc/lc-apply-linear-function lc/tensor-m12 (lc/lc-apply-linear-function lc/tensor-antipode-otimes-id (coproduct sw-1)))))
```


There is also a linear combination "plus".
Its is a linear combination where the coefficients themselves are linear combinations.

That is {"a" {"x" 5, "y" 3}, "b" {"x" 17}} stands for the linear combination
(5 * "x" + 3 * "y") * "a" + (17 * "x") * "b".
This is helpful for doing simple symbolic calculations.

```clojure
  TODO example of this
```


## Running the code

Installation is simple if you have [leiningen][http://leiningen.org]; this tool will arrange
to retrieve everything else you need. On Mac OS, for example,

~~~ sh
$ brew install leiningen
~~~

ought to get you started. Clone the repo. In the repo folder, `lein repl` will start a REPL,
where you can run the examples from above.

To run the test suite:

~~~ sh
$ lein test
~~~

Copyright © 2018 FIXME

Distributed under the [Eclipse Public License](https://opensource.org/licenses/eclipse-1.0.php), the same as Clojure.
