# Revision history for htdp-image

## 1.1.0.0 -- 2019-07-25

* Add "Htdp" to submodules


## 1.0.0.0 -- 2019-07-21

* Remove emptyScene.
* Added abovesAlign, besidesAlign.
* Added documentation.
* Ready for hackage! (I think...)


## 0.4.2.0 -- 2019-07-21

* Added triangle function family.


## 0.4.1.0 -- 2019-07-20

* Added {overlay, underlay}AlignOffset/Align/XY


## 0.4.0.0 -- 2019-07-20

* placeImage now increases the binding box to fit both images
  (as compared to just assuming that the second image is the base)
* Redefined {above, beside}, {above, beside}Align using placeImageAlign
* Added {overlay, underlay}Align.
* Added a simple implementation of rotate.


## 0.3.1.0 -- 2019-07-17

* Add aboveAlign, besideAlign, placeImageAlign, besides, aboves, and placeImages
* Fix overlay/underlay (assumed that (0, 0) was always the center of an image)

## 0.3.0.0 -- 2019-07-16

* Change coordinate system to screen coordinate system!


## 0.2.6.0 -- 2019-07-16

* Added star
* Provide alias for Solid and Outline


## 0.2.5.0 -- 2019-07-14

* Added overlay and underlay


## 0.2.4.0 -- 2019-07-14

* Added empty-image and rhombus

## 0.2.3.0 -- 2019-07-14

* Added line
* Fixed drawImage (crashed if width or height was zero)


## 0.2.2.0 -- 2019-07-13

* Added rectangle, square, and ellipse
* Access to Image data type


## 0.2.1.0 -- 2019-07-11

* Added triangle


## 0.2.0.0 -- 2019-07-11

* Htdp is now the entry point, giving access to only the functions
  required for creating images.


## 0.1.0.0 -- 2019-07-10

* Basic idea behind the implementation
* Provides utility to draw circles by placing them on top,
  beside, or above each other.
* White empty scene for background
