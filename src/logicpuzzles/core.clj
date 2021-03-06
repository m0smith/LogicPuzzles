(ns logicpuzzles.core
  (:gen-class)
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

(defn rule-5
  "The person with a reservation at 5:00pm didn't subscribe to Time."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [s1 s2]
          (== [_ _ _ 5] s1)
          (== [_ :time _ _] s2)
          (membero s1 answers)
          (membero s2 answers)
          (!= s1 s2))))

(defn rule-6
  "The Cosmopolitan subscriber has an earlier reservation than the mascarpone enthusiast."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [r1 r2]
          (membero [_ :cosmopolitan _ r1] answers)
          (membero [_ _ :mascarpone r2] answers)
          (lefto r1 r2 [5 6 7 7.5 8.5]))))

(defn rule-7
  "Bailey has a later reservation than the blue cheese enthusiast."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [r1 r2]
          (membero [_ _ :blue-cheese r1] answers)
          (membero [:bailey _ _ r2] answers)
          (lefto r1 r2 [5 6 7 7.5 8.5]))))

(defn rule-8
  "Either the person with a reservation at 7:00pm or the person with a
   reservation at 7:30pm subscribed to Fortune."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [r]
          (membero [_ :fortune _ r] answers)
          (conde [(== r 7)]
                 [(== r 7.5)]))))

(defn rule-9
  "Landon has a later reservation than the Time subscriber."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [r1 r2]
          (membero [_ :time _ r1] answers)
          (membero [:landon _ _ r2] answers)
          (lefto r1 r2 [5 6 7 7.5 8.5]))))

(defn rule-10
  "The Fortune subscriber is not Jamari."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [s1 s2]
          (== [_ :fortune _ _] s1)
          (== [:jamari _ _ _] s2)
          (membero s1 answers)
          (membero s2 answers)
          (!= s1 s2))))

(defn rule-11
  "The person with a reservation at 5:00pm loves mozzarella."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (membero [_ _ :mozzarella 5] answers)))

(defn rule-0
  "Other stuff"
  [answers]
  (symbol-macrolet
   [_ (lvar)]
   (fresh [s]
          (== [:amaya _ _ _] s)
          (membero s answers))))
                          
(defn solve []

(show
  (let [people       (repeatedly 5 lvar)
        magazines    (repeatedly 5 lvar)
        cheeses      (repeatedly 5 lvar)
        reservations (repeatedly 5 lvar)
        answers (map list people magazines cheeses reservations)]
    (run 1 [q]
         (== q answers)

         ;(== people [:amaya :bailey :jamari :jason :landon])
         ;(== magazines [:fortune :time :cosmopolitan :us-weekly :vogue])
         ;(== cheeses [:asiago :blue-cheese :mascarpone :mozzarella :muenster])
         ;(== reservations [5 6 7 7.5 8.5])
         (rule-0 answers)
         (rule-1 answers)
          (rule-2 answers)
          (rule-3 answers)
          (rule-4 answers)
          (rule-5 answers)
          (rule-6 answers)
          (rule-7 answers)
          (rule-8 answers)
          (rule-9 answers)
          (rule-10 answers)
          (rule-11 answers)

          ;(permuteo magazines [:fortune :time :cosmopolitan :us-weekly :vogue])
          ;(permuteo cheeses [:asiago :blue-cheese :mascarpone :mozzarella :muenster])
                                        ;(permuteo reservations [5 6 7 7.5 8.5])
          ))))

;;; Time to (run 1 [q]): ~1s.
;;; Time to (run* [q]): ~16s.
;;;
;;; (There is only one solution, but it's significantly faster to stop
;;; as soon as we've found it.)
