#lang stacker/smol/state

(deffun (pause) 0)

(deffun (f y)
  (+ (pause) y))

(deffun (g x)
  (+ (f 6) x ))

(g 4)