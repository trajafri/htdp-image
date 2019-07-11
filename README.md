## htdp-image

htdp-image is a simple graphics library inspired by Racket's htdp/image.

Under the hood, it is currently a wrapper on top of Gloss, another easy to use graphics library but htdp-image makes drawing objects even easier for beginners.

In the future, if Gloss seems to start limit the features this library wishes
to implement, then the newer versions might start using graphics libraries that
work on a lower level than Gloss.

Currently, the htdp-image uses the [combinator pattern](https://wiki.haskell.org/Combinator_pattern) to draw images.

#Examples:

To draw four circles beside each other:

```haskell
drawImage $ foldr1 beside $ replicate 5 (circle 20 Solid red)
```

![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/beside.png "Four circles beside each other")


To draw four circles above each other:

```haskell
drawImage $ foldr1 above $ replicate 5 (circle 20 Solid red)
```
 
![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/above.png "Four circles above each other")


To draw two circles stack on top:

```haskell
drawImage $ above (beside redCirc blueCirc) (beside blueCirc redCirc)
 where
  redCirc  = (circle 20 Solid red)
  blueCirc = (circle 20 Solid blue)
```

![alt text](https://raw.githubusercontent.com/trajafri/htdp-image/master/example-images/above-beside.png "Two circles stacked on each other")
