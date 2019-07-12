# htdp-image

htdp-image is a simple graphics library inspired by [Racket](https://racket-lang.org/)'s [2htdp/image](https://docs.racket-lang.org/teachpack/2htdpimage.html) library.

Under the hood, it is currently a wrapper on top of Gloss, another easy to use graphics library, but htdp-image
makes drawing objects even easier for beginners.

In the future, if it seems like Gloss is limiting the features this library wishes
to implement, then the newer versions might start using graphics libraries that
work on a lower level than Gloss.

This library uses the [combinator pattern](https://wiki.haskell.org/Combinator_pattern) to draw images.

## Examples:

To draw five circles beside each other:

```haskell
drawImage $ foldr1 beside $ replicate 5 (circle 20 Solid red)
```

![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/beside.png "Four circles beside each other")


To draw five circles above each other:

```haskell
drawImage $ foldr1 above $ replicate 5 (circle 20 Solid red)
```
 
![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/above.png "Four circles above each other")


To draw four circles stacked on top of each other (2 on 2):

```haskell
drawImage $ above (beside redCirc blueCirc) (beside blueCirc redCirc)
 where
  redCirc  = (circle 20 Solid red)
  blueCirc = (circle 20 Solid blue)
```

![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/above-beside.png "Two circles stacked on each other")


To draw four iterations of the sierpinski triangle (don't try super big iterations!):

```haskell
drawImage $ foldr (const sier) (triangle 20 Solid red) [1, 2, 3, 4]
 where
  sier t = above t $ beside t t
```

![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/sierpinski.png "Sierpinski 4")

