##  assert functions

`assert-comparison!` is a `defrules`, it compares two expressions
using a predicate function. It returns nothing if the two expressions
satify  predicate function(`#t`), otherwise throws.
` <- pred expr1 expr2`

```
> (assert-comparison! = 9 9)
```

`assert-equal!` is a `defrules`, it compares two expressions
It returns nothing if the two expressions are equal, otherwise throws.
` <- expr1 expr2`

```
> (assert-equal! 9 9)
```

`assert-comparison-helper!` is a helper function, it compares two expressions
using a predicate function. It throws error message when predicate functions
return `#f`.
` <- pred-name expr1 expr2 pred val1 val2`
pred-name, expr1 and expr2 and symbols. pred is predicate function while
val1 and val2 are expressions

```
> (assert-comparison-helper! 'equal? '2 '3 equal? 2 3)
*** ERROR IN assert-comparison-helper!, "assert.ss"@18.5 -- equal? 2 3 2 3
```

```
> (assert-comparison-helper! 'equal? '2 '2 equal? 2 2)
```
