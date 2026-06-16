## Decoding CSV, with byte buffers instead of character strings

`bcsv-options` is the main data structure used in decoding CSV. It comes with 
accessor functions and set(statefull) functions
```
;;; Parameters
(defstruct bcsv-options
  (separator ;; byte ;; Separator between CSV fields, usually #\, as in the name; sometimes #\tab.
   quote ;; byte ;; delimiter of string data; pascal-like quoted as double itself in a string.
   unquoted-quotequote? ;; bool ;; does a pair of quotes represent a quote outside of quotes?
                                ;; M$, RFC says no, csv.3tcl says yes
   loose-quote? ;; bool ;; can quotes appear anywhere in a field?
   allow-binary? ;; bool ;; do we accept non-ascii data?
   eol ;; string ;; what eol value do we output?
   accept-lf? ;; bool ;; is lf valid line-ending on input?
   accept-cr? ;; bool ;; is cr valid line-ending on input?
   accept-crlf? ;; bool ;; is crlf valid line-ending on input?
   skip-whitespace?) ;; bool ;; shall we skip unquoted whitespace around separators?
  transparent: #t)
  ```
The below are the accessor functions:

`bcsv-options-separator`
`bcsv-options-quote`
`bcsv-options-unquoted-quotequote?`
`bcsv-options-loose-quote?`
`bcsv-options-allow-binary?`
`bcsv-options-eol`
`bcsv-options-accept-lf?`
`bcsv-options-accept-cr?`
`bcsv-options-accept-crlf?`
`bcsv-options-skip-whitespace?`

The below are the set(statefull) functions:

`bcsv-options-separator-set!`
`bcsv-options-quote-set!`
`bcsv-options-unquoted-quotequote?-set!`
`bcsv-options-loose-quote?-set!`
`bcsv-options-allow-binary?-set!`
`bcsv-options-eol-set!`
`bcsv-options-accept-lf?-set!`
`bcsv-options-accept-cr?-set!`
`bcsv-options-accept-crlf?-set!`
`bcsv-options-skip-whitespace?-set!`

`make-bcsv-options` function creates a new `bcsv-options` struct.

`current-bcsv-options` is `make-parameter` of `creativyst-bcsv-options`.

`creativyst-bcsv-options` is an instance of `bcsv-options`
`(make-bcsv-options (b #\,) (b #\") #f #f #t +bcrlf+ #t #t #t #t)`

`strict-rfc4180-bcsv-options` is an instance of `bcsv-options`
`(make-bcsv-options (b #\,) (b #\") #f #f #f +bcrlf+ #f #f #t #f)`

`rfc4180-bcsv-options` is an instance of `bcsv-options`
`(make-bcsv-options (b #\,) (b #\") #f #f #t +blf+ #t #f #t #f)`

`call-with-rfc4180-bcsv-syntax` takes a function as its only input.
This function is called a `thunk`, it is a zero argument function.
Its application happens inside `parameterize` which changes
the fields of bcsv-option with `rfc4180-bcsv-options`
` Any <- Function(with zero argument)`

`with-rfc4180-bcsv-syntax` is a `defrule`. Its arguments are 
wrap inside a `lambda` as `thunk` before `call-with-rfc4180-bcsv-syntax`
application.
` Any <- Any`


`call-with-strict-rfc4180-bcsv-syntax` takes a function as its only input.
This function is called a `thunk`, it is a zero argument function.
Its application happens inside `parameterize` which changes
the fields of bcsv-option with `strict-rfc4180-bcsv-options`
` Any <- Function(with zero argument)`

`with-strict-rfc4180-bcsv-syntax` is a `defrule`. Its arguments are 
wrap inside a `lambda` as `thunk` before `call-with-strict-rfc4180-bcsv-syntax`
application.
` Any <- Any`

`call-with-creativyst-bcsv-syntax` takes a function as its only input.
This function is called a `thunk`, it is a zero argument function.
Its application happens inside `parameterize` which changes
the fields of bcsv-option with `creativyst-bcsv-options`
` Any <- Function(with zero argument)`

`with-creativyst-bcsv-syntax` is a `defrule`. Its arguments are 
wrap inside a `lambda` as `thunk` before `call-with-creativyst-bcsv-syntax`
application.
` Any <- Any`


### Read Bytes from CSV

`read-bcsv-line`
Read one line from PORT(byte) in CSV format, using the current syntax parameters.
Return a list of bytes, one entry for each line,
`[Bytes] <- Port(Byte)`

```
> (def mport (open-input-bytes (string->bytes "1997, Ford, E350\n 2020, Volvo, V30i")))
> (read-bcsv-line foo)                                                               
(#u8(49 57 57 55) #u8(70 111 114 100) #u8(69 51 53 48))
```


`read-bcsv-lines`
Read lines from PORT(byte) in CSV format, using the current syntax parameters.
Return a list of list of bytes, one entry for each line,
`[[Bytes]] <- Port(Byte)`

```
> (def mport (open-input-bytes (string->bytes "1997, Ford, E350\n 2020, Volvo, V30i")))
> (read-bcsv-lines mport)                                                              
((#u8(49 57 57 55) #u8(70 111 114 100) #u8(69 51 53 48)) (#u8(50 48 50 48) #u8(86 111 108 118 111) #u8(86 51 48 105)))
```

`read-bcsv-file`
Open the file designated by the path, using the provided settings if any,
and call read-bcsv-lines on it.
`[[Bytes]] <- File`

```
;; Let test.txt contain
1997, Ford, E350
2020, Volvo, V30i
> (read-bcsv-file "test.txt")                                                              
((#u8(49 57 57 55) #u8(70 111 114 100) #u8(69 51 53 48)) (#u8(50 48 50 48) #u8(86 111 108 118 111) #u8(86 51 48 105)))
```

`write-bcsv-lines`
Given a list of LINES, each of them a list of fields, and a PORT,
format those lines as CSV according to the current syntax parameters.
Field could be string, number, symbol, bytes, and nothing
` <- [[Field]] Port`

```
> (def lines                                                            
((#u8(49 57 57 55) #u8(70 111 114 100) #u8(69 51 53 48)) (#u8(50 48 50 48) #u8(86 111 108 118 111) #u8(86 51 48 105))))
> (def mport (open-output-bytes))
> (write-bcsv-lines lines mport)
;; To see the content of the port, try this
> (get-output-bytes mport) 
```

`write-bcsv-file`
Given File and a list of LINES, each of them a list of fields, and a PORT,
format those line as CSV according to the current syntax parameters, 
Field could be string, number, symbol, bytes, and nothing. It would write
CSV to file
` <- File [[Field]]`


```
;; Let test.txt contain
1997, Ford, E350
2020, Volvo, V30i
> (def lines 
(read-bcsv-file "test.txt")) 
> lines                                                             
((#u8(49 57 57 55) #u8(70 111 114 100) #u8(69 51 53 48)) (#u8(50 48 50 48) #u8(86 111 108 118 111) #u8(86 51 48 105)))
;; Create file newtest.txt
> (write-bcsv-file "newtest.txt" lines)
;;$ cat newtest.txt
1997,Ford,E350#u8(13 10)2020,Volvo,V30i#u8(13 10)
```
