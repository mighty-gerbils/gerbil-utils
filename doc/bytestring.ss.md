
## Bytes as strings

`byte?` returns `#t` for byte(0 to 255).
`Boolean <- Byte`

```
> (byte? 0)
#t
> (byte? 254)
#t
> (byte? 255)
#t
> (byte? 256)
#f
```

`bytes-value-index`, take `val`(value) of interest, `bs bytes`, `start` and `end`.
If `val` is found within the range of `start` and `end` of `bs` bytes,
it returns the index, otherwise `#f`.
`(Or Integer #f) <- Integer Bytes (Optional Integer) (Optional Integer)`

```
;; (bytes-value-index val bs (start 0) (end (bytes-length bs)))
> (bytes-value-index 59 #u8(211 112 58 90 59 101 98 200) )
4
> (bytes-value-index 79 #u8(211 112 58 90 59 101 98 200) )
#f
```

`bytes-index`, takes a pred?(function) and any number of bytes.
Returns exact nonnegative integer or `#f`
Finds and returns the index of the first elements in bytes1 bytes2 ··· that satisfy pred?. If no matching element is found by the end of the shortest bytes, `#f` is returned.
`(Or Integer #f) <- Function (One or more Bytes)`

If `val` is found within the range of `start` and `end` of `bs` bytes,
it returns the index, otherwise `#f`.
`(Or Integer #f) <- Integer Bytes (Optional Integer) (Optional Integer)`


```
> (bytes-index even? #u8(3 1 4 1 5 9))
2
> (bytes-index < #u8(3 1 4 1 5 9 2 5 6) #u8(2 7 1 8 2))
1
> (bytes-index = #u8(3 1 4 1 5 9 2 5 6) #u8(2 7 1 8 2))
#f 
```