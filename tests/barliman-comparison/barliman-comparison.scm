(load "test-proofo.scm")
(load "test-fib-aps-synth.scm")
(load "challenge-7.scm")

;; Testing "proofo 1"
;; (time (test "proofo 1" ...))
;;     96 collections
;;     0.203883250s elapsed cpu time, including 0.019870000s collecting
;;     0.204366000s elapsed real time, including 0.020072000s collecting
;;     806451296 bytes allocated, including 805389024 bytes reclaimed
;; Testing "proofo 2"
;; (time (test "proofo 2" ...))
;;     1835 collections
;;     9.074017792s elapsed cpu time, including 2.025765000s collecting
;;     9.128629000s elapsed real time, including 2.044275000s collecting
;;     15400070672 bytes allocated, including 15331553680 bytes reclaimed

;; Testing "peano-synth-fib-aps 3"
;; (time (test "peano-synth-fib-aps 3" ...))
;;     5238 collections
;;     5.030516458s elapsed cpu time, including 0.215702000s collecting
;;     5.047474000s elapsed real time, including 0.219223000s collecting
;;     44005126480 bytes allocated, including 43998058864 bytes reclaimed
;; Testing "peano-synth-fib-aps 4"
;; (time (test "peano-synth-fib-aps 4" ...))
;;     10584 collections
;;     10.988272749s elapsed cpu time, including 0.560247000s collecting
;;     11.114258000s elapsed real time, including 0.579978000s collecting
;;     88926770576 bytes allocated, including 88917328088 bytes reclaimed

;; Give this a few minutes.
;; Testing scheme-in-scheme-quine-with-quasiquote
;; (time (test (quote scheme-in-scheme-quine-with-quasiquote) ...))
;;     6074 collections
;;     118.053622333s elapsed cpu time, including 38.118214000s collecting
;;     119.845358000s elapsed real time, including 38.706830000s collecting
;;     50944112800 bytes allocated, including 50398500592 bytes reclaimed
;; >
