## Basic Bytes functionalities


`bytes-reverse` 
Given bytes, perform byte reverse.

`Bytes <- Bytes`
```
> (bytes-reverse #u8(50 48 50 48))
#u8(48 50 48 50)
```


`bytes-prefix?` 
This checks if `b bytes` start with `pre bytes`,
if yes return `#t`, otherwise returns `#f`.
`Bytes <- Bytes Bytes`

```
;; (bytes-prefix? pre b)
> (bytes-prefix? #u8(50 48 50 48) #u8(50 48 50 48 117 67 89 90 32 37))
#t
```

```
;; (bytes-prefix? pre b)
> (bytes-prefix? #u8(211 100 50 48) #u8(50 48 50 48 117 67 89 90 32 37))
#f
```