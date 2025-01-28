When we write libraries for others (including ourselves) to use we often require some preconditions to be met. Sometimes we just make them implicit (in the form of folklore, passed from developer to developer during post mortem ritual). In other cases we encode the possibility of failure due to unsatisfied preconditions.

``` haskell
safeHead :: [a] -> Maybe a
safeHead [] = None
safehead (a:_) = Just a
```

Is this is the only thing we can do? Definitely no! Is it the best thing we can do? Depends on the situation.

This article is part of the readings series where I take one topic and share links to related articles and papers. This time our focus is precondition encoding.

<!--more-->

In [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) article[^1] Alexis King describes the meaning of type-driven design and explains the "parse, don't validate" slogan using simple code examples. The most prominent one is comparison of the following two functions. Alexis then elaborates more on the differences between them.

``` haskell
validateNonEmpty :: [a] -> IO ()
validateNonEmpty (_:_) = pure ()
validateNonEmpty [] = throwIO $ userError "list cannot be empty"

parseNonEmpty :: [a] -> IO (NonEmpty a)
parseNonEmpty (x:xs) = pure (x:|xs)
parseNonEmpty [] = throwIO $ userError "list cannot be empty"
```

I really like that in the parse approach the learned information is not discarded and can be used later, while validate approach completely discards what it learned.

Alexis also mentions one important thing. Using `Maybe` as resulting type of safe functions means that it's easier to implement safe functions, but it pushes responsibility of the failure to the call site significantly increasing complexity of safe function user. This issue is talked through and Alexis provides a solution. You just need to be stricter about your input, that's where all the static typing helps you. [Matt Parsons](https://www.parsonsmatt.org/about/) has a wonderful [blog post](https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html) on this topic as well.

``` haskell
safeHead :: NonEmpty a -> a
safeHead (x :| xs) = x
```

With this approach, users of `safeHead` must handle failure ahead of time. But this also means that the calling code doesn't need to use things like `fromJust` when it is genuinely known that the list is non empty.

There is an orthogonal view on this exact problem by [Matt Noonan](https://storm-country.com/) which he explains in the [Ghosts of Departed Proofs (Functional Pearl)](https://kataskeue.com/gdp.pdf) paper. Matt describes an approach where preconditions (like non-empty list or key existence in the map) are encoded in the Haskell type system as phantoms. Users must prove that preconditions are met using exported set of lemmas.

One of my favourite examples from this paper is the function that computes the dot product of the vector and its reverse. It can be implemented in terms of a regular dot product function, which has one important precondition - both vectors must share the same length. So we encode it on the type level.

``` haskell
-- regular dot product
dot :: ([Double] ~~ vec1 ::: Length vec1 == n)
    -> ([Double] ~~ vec2 ::: Length vec2 == n)
    -> Double
dot v1 v2 = sum (zipWith (*) v1 v2)
```

You can read it as 'give me some list which I call `vec1` such that its length is equal to `n` and then give me some other list which I call `vec2` such that its length is also equal to `n` and I will give you some number back'. As you can see, it's much more verbose compared to `dot :: [Double] -> [Double] -> Double`, but it caries an important requirement on the type level, so no value is accidentally discarded by `zipWith`. Now if we want to write a function that calculates the dot product of vector and it's reverse we need to provide a proof that the length of reversed list is equal to the original lists' length.

``` haskell
dot_rev :: [Double] -> Double
dot_rev xs = name xs $
  \vec -> dot (vec ...refl) (reverse vec ...rev_length)
```

First thing that we notice is that this function doesn't have any preconditions, it works with any vector. And then there are `name`, `...refl` and `...rev_length` gluing the `dot` function and it's arguments: original vector and its reverse. I am not going to explain everything (this is what paper does after all), my goal is to interest you enough so that you read the paper.

In short, `name` is the way to give some fixed name to it's argument and then evaluate a function that takes a named argument. It uses Rank-2 scoping (the trick from ST[^2]). Now since we have a named vector we need to prove that `vec` and `reverse vec` share the same length. So we need to provide a proof that the length of the first vector is equal to some `n` and then prove that the length of the second vector is equal to the same `n`. First proof is provided by reflection. `...` is just an operator which gives a way to attach a proof. The second vectors length is proven by lemma (or in this case, axiom) called `rev_length`, and it's implementation is simple.

``` haskell
rev_length :: Proof (Length (Rev xs) == Length xs)
rev_length = axiom
```

It just says that there is an axiom that says that the length of the list is equal to the length of its reverse. And we use it to make the compiler happy. The meaning of `Length` and `Rev` are explained in the… paper. So please take a look!

It's also interesting that all this machinery is implemented using Phantom Types and coercions, so compiler discards a lot of stuff from the runtime (like naming and proof providing), which means that there is no run-time penalty on having compile-time guarantees. We already [touched this topic previously](/posts/2020-01-20-predicate-composition).

Now, it's up to you how far to go into encoding preconditions. I think that in many cases it's enough to use proper data types. In some very specific scenarios it's easier (or more convenient) to use ghosts of departed proofs approach. But always keep in mind, that it's possible to move constraints into input type instead of wrapping result into `Maybe` (or alike). As people say, make illegal states unrepresentable.

Stay safe!

# References

1.  Alexis King. Parse, don’t validate. <https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/>, 2019.
2.  Alexis King. No, dynamic type systems are not inherently more open. <https://lexi-lambda.github.io/blog/2020/01/19/no-dynamic-type-systems-are-not-inherently-more-open/>, 2020.
3.  Haskell Wiki. Monad/ST. <https://wiki.haskell.org/Monad/ST>, 2012. Accessed: 2020-01-28.
4.  Sandy Maguire. Thinking with Types. <https://leanpub.com/thinking-with-types>, 2018.
5.  Matt Parsons. Type Safety Back and Forth. <https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html>, 2017.
6.  M. Noonan. Ghosts of departed proofs. , 2018. Accessed: 2020-01-28.
7.  Boris Buliga. Predicate composition. <https://d12frosted.io/posts/2020-01-20-predicate-composition.html>, 2020.

[^1]: If you liked this post, please make sure to read the next part '[No, dynamic type systems are not inherently more open](https://lexi-lambda.github.io/blog/2020/01/19/no-dynamic-type-systems-are-not-inherently-more-open/)' which is an open answer to some of the original article' comments.

[^2]: ST monad enables pure computations with local mutable variable that is not exposed (leaked) to the outside. It uses Rank-2 trick to enforce the scope of the variable on the compile time. You can read more about ST on [Haskell Wiki](https://wiki.haskell.org/Monad/ST), [Stackoverflow](https://stackoverflow.com/questions/12468622/how-does-the-st-monad-work), but the best explanation can be found in [Thinking with Types](https://leanpub.com/thinking-with-types) book by [Sandy Maguire](https://reasonablypolymorphic.com/), Chapter 7.2 Scoping Information with Existentials.
