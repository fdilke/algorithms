#lang racket/gui

(require racket/draw net/url)

(define IMAGE-of-UFO 
    (read-bitmap (get-pure-port (string->url "http://racket-lang.org/logo.png")))
)