(in-package :software-evolution)
(setq
 *MAX-POPULATION-SIZE* 100         ;; Maximum allowable population size.
 *POS-TEST-NUM*        10          ;; Number of positive tests.
 *NEG-TEST-NUM*        1           ;; Number of negative tests.
 *POS-TEST-MULT*       1           ;; Multiplier for positive test cases
 *NEG-TEST-MULT*       1           ;; Multiplier for negative test cases
 *CROSS-CHANCE*        1/5         ;; Fraction of new individuals generated using crossover rather than mutation.
 *PATHS* '((:neg . "sample.neg")
           (:pos . "sample.neg"))  ;; List of pairs of the form '(keyword . "sample-file").
 *SEED-SOFT*           "gcd.s"     ;; File holding a seed individual
 *SAVE-SOFT*           "fix.s"     ;; File to hold any potential individual returned by `repair'
 *SAVE-POPULATION*     "pop.store" ;; Save the final population here.
 MAX-EVALS             100         ;; quit after this many fitness evaluations
 MAX-FIT               11          ;; quit when an individual achieves this fitness or higher
 )
