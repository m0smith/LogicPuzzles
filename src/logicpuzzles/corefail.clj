(ns logicpuzzles.corefail
  (:refer-clojure :exclude [== !=])
  (:use clojure.core.logic
        [clojure.tools.macro :only [symbol-macrolet]]
        clojure.pprint))

(defn show
  [x]
  (time (pprint [x (java.util.Date.)])))

(defne lefto
  "x appears to the left of y in collection l."
  [x y l]
  ([_ _ [x . tail]] (membero y tail))
  ([_ _ [_ . tail]] (lefto x y tail)))

(defn rule-1
  "Of Landon and Jason, one has the 7:30pm reservation and the other loves mozzarella."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [c1 r1 c2 r2]
          (membero [:landon _ c1 r1] answers)
          (membero [:jason _ c2 r2] answers)
          (conde
           [(== r1 7.5)
            (== c2 :mozzarella)]
           [(== r2 7.5)
            (== c1 :mozzarella)]))))

(defn rule-2
  "The blue cheese enthusiast subscribed to Fortune."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
         (membero [_ :fortune :blue-cheese _] answers)))

(defn rule-3
  "The muenster enthusiast didn't subscribe to Vogue."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [s1 s2]
          (== [_ :vogue _ _] s1)
          (== [_ _ :muenster _] s2)
          (membero s1 answers)
          (membero s2 answers)
          (!= s1 s2))))

(defn rule-4
  "The 5 people were the Fortune subscriber, Landon, the person with a
   reservation at 5:00pm, the mascarpone enthusiast, and the Vogue
   subscriber."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (permuteo [[_ :fortune _ _]
              [:landon _ _ _]
              [_ _ _ 5]
              [_ _ :mascarpone _]
              [_ :vogue _ _]]
             answers)))


(defn rule-6
  "The Cosmopolitan subscriber has an earlier reservation than the mascarpone enthusiast."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [r1 r2]
          (membero [_ :cosmopolitan _ r1] answers)
          (membero [_ _ :mascarpone r2] answers)
          (lefto r1 r2 [5 6 7 7.5 8.5]))))


(defn rule-9
  "Landon has a later reservation than the Time subscriber."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [r1 r2]
          (membero [_ :time _ r1] answers)
          (membero [:landon _ _ r2] answers)
          (lefto r1 r2 [5 6 7 7.5 8.5]))))



(defn rule-0
  "Other stuff"
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [s]
          (== [:amaya _ _ _] s)
          (membero s answers))))
                          

(show
      (run 1 [answers]
           
           (rule-0 answers)
           
           (rule-1 answers)
           (rule-2 answers)
           (rule-3 answers)
           (rule-4 answers)
           
           (rule-6 answers)
           (rule-9 answers)
           
           ))

;;; Time to (run 1 [q]): ~1s.
;;; Time to (run* [q]): ~16s.
;;;
;;; (There is only one solution, but it's significantly faster to stop
;;; as soon as we've found it.)
