# Feature List

Only the following 2htdp/image functions have been ported so far:

* [above](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._above%29%29)
* [beside](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._beside%29%29)
* [circle](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._circle%29%29)
* [empty-scene](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._empty-scene%29%29)
* [image-width](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._image-width%29%29)
* [image-height](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._image-height%29%29)
* [place-image](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._place-image%29%29)


# Differences

Some differences are a side effect of using Gloss as the base of this library.

* The default coordinate system is Cartesian instead of the Screen Coordinate System.
* Usually, 2htdp/image is used on the DrRacket IDE, which treats images as bitmap images
  (and even draws them there!).
  htdp-image doesn't have that feature so every image is drawn in a new window.
* empty-scene is basically the same as a rectangle.
