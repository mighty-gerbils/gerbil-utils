
## Converts Integer to Char

`digit-char` would also return `#f` if your input is not within the 
allowed range.
Its default base is 10, but it allows base 1 to 36
It default character case is lower case, but with a `#t` one can 
change this to upper case. Always assume ASCII.
`(Or Char '#f) <- Integer (Optional Integer 10) (Optional Bool #f)`
```
Converting to lower case
> (digit-char 20 36)
#\k
```
```
Converting to upper case
> (digit-char 20 36 #t)
#\K
```
## Converts Integer with base to string

`string<-integer-base` would throw if base is not between 2 and 36.
Its default base is 10.
`String <- Integer (Optional Integer 10)`
```
Using default base 10
> (string<-integer-base 11)  
"11"
```

```
Using default base 2
> (string<-integer-base 11 2)  
"1011"
```

```
Using default base 2
> (string<-integer-base 90 36)  
"2i"
```