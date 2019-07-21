# Feature List

Only the following 2htdp/image functions have not been ported:

## Basic Images

* [add-curve](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._add-curve%29%29)
	This needs more work so leaving this for when I feel like giving this a shot.
* [add-solid-curve](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._add-solid-curve%29%29)
	Same reason as above.
* [text](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._text%29%29)
	Can't really use Gloss' text. Not sure how to determine dimensions. Seems difficult right now.
* [text/font](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._text%2Ffont%29%29)
	Same as above + not sure about fonts.

## Polygons 

* [star (solid)](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._star%29%29)
	Current implementation of a solid star is affected by OpenGL's non-convex polygon issue. It can be solved by making star
  out of convex polygons but needs more work.
* [star-polygon](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._star-polygon%29%29)
* [radial-star](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._radial-star%29%29)
* [regular-polygon](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._regular-polygon%29%29)
* [pulled-regular-polygon](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._pulled-regular-polygon%29%29)
* [polygon](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._polygon%29%29)
	Need to feel motivated to implement these. Just lazy right now. However, pulled polygon seems like more work, but if I get curves working,
  this should also be doable.
* [add-polygon](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._add-polygon%29%29)
	placeImage currently behaves similar to this function. Just need to implement polygon to implement this function.
* [scene+polygon](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.scene%2Bpolygon%29%29)
	Since cropping is not supported, this will not be implemented.

## Placing Images & Scenes

* [scene+line](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.scene%2Bline%29%29)
* [scene+curve](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.scene%2Bcurve%29%29)
	Needs cropping.

## Rotating, Scaling, Flipping, Cropping, and Framing Images

* [rotate](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.rotate%29%29)
	Currently, rotating is glitchy in some cases. Fixing this might need some work.
* [scale](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.scale%29%29)
* [scale/xy](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.scale%2Fxy%29%29)
	Scaling with the current implementation doesn't seem doable.
* [flip-horizontal](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.flip-horizontal%29%29)
* [flip-veritcal](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.flip-veritcal%29%29)
	Since rotate is glitchy, I haven't done this yet + veritcal flip needs some thinking. If it needs scaling, then can't be done right now.
* [crop](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.crop%29%29)
* [crop/align](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.crop%2Falign%29%29)
	Cropping will need more control over the images. Since this package depends on Gloss to handle images, this seems difficult.
* [frame](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.frame%29%29)
* [color-frame](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.color-frame%29%29)
	These are trivial.

## Bitmaps

None are implemented since I was more focused on being able to combine images similar to 2htdp/image

## Image Properties

* [image-baseline](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29.image-baseline%29%29)
	This is used for text which don't exist in this library. No need for this yet.

## Image Predicates

Don't need em!

## Pinholes

Haven't looked into these yet

## Exporting Images to Disk

Maybe. I think Gloss allows something like this


# Differences

* Usually, 2htdp/image is used on the DrRacket IDE, which treats images as bitmap images
  (and even draws them there!).
  htdp-image doesn't have that feature so every image is drawn in a new window.
* empty-scene is basically the same as a rectangle (or, there is no concept of scene as it is in 2htdp/image).
  In later versions, this will be removed.
* Non-convex polygons are glitchy, thanks to OpenGL. Something will be done to fix non-convex polygons
  provided by htdp-image, but if a user creates a non-convex polygon, then they might run into OpenGL issues.
* placeImage does not crop part of images that lay outside the dimensions of the image they are being placed on top of.
  In other words, placeImage is similar to add-polygon function.
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
  the (width X height) binding box, not the triangle. Therefore, the rotated image ends up taking up more room
  than it should.
* Instead of having "x-place" and "y-place", there is one Alignment type with 3 members (Low, Mid, and High).
