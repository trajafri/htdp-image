# Feature List

Only the following 2htdp/image functions have been ported so far:

* [above](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._above%29%29)
* [above/align](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._above%2Falign%29%29)
* [beside](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._beside%29%29)
* [beside/align](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._beside%2Falign%29%29)
* [circle](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._circle%29%29)
* [ellipse](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._ellipse%29%29)
* [empty-image](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._empty-image%29%29)
* [empty-scene](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._empty-scene%29%29)
* [image-width](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._image-width%29%29)
* [image-height](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._image-height%29%29)
* [line](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._line%29%29)
* [overlay](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%29%29)
* [overlay/align](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%2Falign%29%29)
* [overlay/offset](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%2Foffset%29%29)
* [overlay/align/offset](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%2Falign%2Foffset%29%29)
* [overlay/xy](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%2Fxy%29%29)
* [place-image](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._place-image%29%29)
* [place-images](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._place-images%29%29)
* [place-image/align](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._place-image%2Falign%29%29)
* [rectangle](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._rectangle%29%29)
* [rhombus](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._rhombus%29%29)
* [rotate](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._rotate%29%29)
* [square](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._square%29%29)
* [star](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._star%29%29)
* [triangle](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._triangle%29%29)
* [underlay](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._underlay%29%29)
* [underlay/align](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._underlay%2Falign%29%29)
* [underlay/align/offset](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._underlay%2Falign%2Foffset%29%29)
* [underlay/offset](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._underlay%2Foffset%29%29)
* [underlay/xy](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._underlay%2Fxy%29%29)

# Differences

* Usually, 2htdp/image is used on the DrRacket IDE, which treats images as bitmap images
  (and even draws them there!).
  htdp-image doesn't have that feature so every image is drawn in a new window.
* empty-scene is basically the same as a rectangle (or, there is no concept of scene as it is in 2htdp/image).
* Non-convex polygons are glitchy, thanks to OpenGL. Something will be done to fix non-convex polygons
  provided by htdp-image, but if a user creates a non-convex polygon, then they might run into OpenGL issues.
* placeImage does not crop part of images that lay outside the dimensions of the image they are being placed on top of.
* Since placeImage does not crop parts of image out of bounds, we have two options.
  - We ignore the parts out of bounds
  - We increase the bounds so that the binding box contains both images
  htdp-image chooses the second option, which results in placeImage being a function that can be used to implement
  all combinator functions.
* I don't think cropping will be supported, unless there is some hacky way of simulating cropping.
* Non rectangular shapes, when rotated, might have bigger binding box than they should be. This is because
  the new binding box is currently calculated by using the old binding box.
  For example, if we have an equilateral triangle, the binding box is originally (width X height) of the triangle.
  Then let's say we rotate the triangle by 30 degrees. In this case, the new binding box will be big enough to fit
  the (width X height) binding box, not the triangle.
