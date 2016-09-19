#lang s-exp "../track.rkt"
(struct-copy style style:classic
             [scales/e (fin/e scale-harmonic-minor)]
             [tempo/e (fin/e 120)])
