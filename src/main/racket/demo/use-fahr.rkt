#lang racket
 
(require "fahrenheit.rkt")

(println "Hello Racket, I can find a submodule!")
(println (string-append "conversion yields: " 
    (~v (fahrenheit->celsius 32))
    " "
    (~v (* 1.0 (fahrenheit->celsius 451)))
))

