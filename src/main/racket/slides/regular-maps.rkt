#lang slideshow ; or slideshow/widescreen

(require 2htdp/universe )
(require 2htdp/image)

 (define IMAGE-of-UFO 
    (bitmap/file "ufo-pic.png")
)

(slide
 #:title "Empty People"
 (t "synchronized squawking")
 (tt "repetition is truth")
 (it "may the loudest voice be heard")
 (item "Fourth step")
 (rt "who needs serifs anyway?")
 (titlet "Proudly presenting a void")
 (para
    (rt "you see, we're total frauds")
    (titlet "who don't do anything")
  )
    IMAGE-of-UFO
 )

 (slide
 #:title "Example"
 (item "First step")
 'next
 (item "Second step")
 'next
 'alts
 (list (list (item "Tentative third step")
             'next
             (item "This isn't working... back up"))
       (list (item "Third step that works")))
 'next
 (item "Fourth step"))

(slide
 #:title "All about Regular Maps"
 (t "hyperbolic adventures")
 (t "with Buekenhout geometries and von Dyck groups")
 (t "weird geometry FTW!")
)

(slide
 #:title "How to Say Hello"
 (item "If you want to create an example, you"
      "can always do something with" (bt "Hello World!"))
 (item "It's a bit silly, but a follow-up example"
       "could be" (bt "Goodbye Dlrow!")))
