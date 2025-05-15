#lang racket/gui

;; (require 2htdp/universe 2htdp/image)
;; (require racket/draw net/url)
(require 2htdp/universe )
(require 2htdp/image)


(define WIDTH 1000)
(define HEIGHT 500)

;; ufo pic
(define IMAGE-of-UFO 
    (bitmap/file "ufo-pic.png")
)


(define (add-3-to-state current-state)
    (+ current-state 3)
)

(define (draw-a-ufo-onto-an-empty-scene current-state)
    (place-image
        IMAGE-of-UFO (/ WIDTH 2) current-state
        (empty-scene WIDTH HEIGHT)
    )
)

(define (state-is-300 current-state)
    (>= current-state 300)
)

(big-bang 0
    (on-tick add-3-to-state)
    (to-draw draw-a-ufo-onto-an-empty-scene)
    (stop-when state-is-300)
)

