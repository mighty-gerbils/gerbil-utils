## Basic parser functions

This assumes Latin / English alphabet

`byte-ascii-alphabetic?` returns Boolean.
`Boolean <- Integer`

```
> (byte-ascii-alphabetic? 90)
#t
```

```
> (byte-ascii-alphabetic? 900)
#f
```

`char-ascii-alphabetic?` returns Boolean.
`Boolean <- Char`

```
> (char-ascii-alphabetic? #\B)
#t
```

```
> (byte-ascii-alphabetic? #\!)
#f
```

`byte-ascii-numeric?` returns Boolean.
For `#t` input should be between 48 and 57.
`Boolean <- Integer`

```
> (byte-ascii-numeric? 48)
#t
```

```
> (byte-ascii-numeric? 60)
#f
```

`char-ascii-numeric?` returns Boolean.
For `#t` input should be between 0 and 9.
`Boolean <- Char`

```
> (char-ascii-numeric? #\0)
#t
```

```
> (char-ascii-numeric? #F)
#f
```

`byte-ascii-alphanumeric?` returns Boolean.
`Boolean <- Integer`

```
> (byte-ascii-alphanumeric? 122)
#t
```

```
> (byte-ascii-alphanumeric? 200)
#f
```

`char-ascii-alphanumeric?` returns Boolean.
For `#t` input should be between #\a and #\z or #\A and #\Z.
`Boolean <- Char`

```
> (char-ascii-alphanumeric? #\a)
#t
```

```
> (char-ascii-alphanumeric? #\{)
#f
```

`byte-ascii-alphanumeric-or-underscore?` returns Boolean.
`Boolean <- Integer`

```
> (byte-ascii-alphanumeric-or-underscore? 95)
#t
```

```
> (byte-ascii-alphanumeric-or-underscore? 93)
#f
```

`char-ascii-alphanumeric-or-underscore?` returns Boolean.
`Boolean <- Char`

```
> (char-ascii-alphanumeric-or-underscore? #\_)
#t
```

```
> (char-ascii-alphanumeric-or-underscore? #\!)
#f
```

`byte-ascii-graphic?` returns Boolean.
For `#t` input should be between 32 and 127.
`Boolean <- Integer`

```
> (byte-ascii-graphic? 32)
#t
```

```
> (byte-ascii-graphic? 128)
#f
```

`char-ascii-graphic?` returns Boolean.
`Boolean <- Char`

```
> (char-ascii-graphic? #\\)
#t
```

```
> (char-ascii-graphic? #\newline)
#f
```

`byte-ascii-whitespace?` returns Boolean.
`Boolean <- Integer`

```
> (byte-ascii-whitespace? #x20)
#t
```

```
> > (byte-ascii-whitespace? #x21)
#f
```

`char-ascii-whitespace?` returns Boolean.
`Boolean <- Char`

```
> (char-ascii-whitespace? #\space)
#t
```

```
> (char-ascii-whitespace? #\a)
#f
```

`byte-ascii-printable?` returns Boolean.
`Boolean <- Integer`

```
> (byte-ascii-printable? #x0D)
#t
```

```
> (byte-ascii-printable? 30)
#f
```

`char-ascii-printable?` returns Boolean.
`Boolean <- Char`

```
> (char-ascii-printable? #\D)
#t
```

```
> (char-ascii-printable? #\backspace)
#f
```

`byte-ascii-digit` returns `(or Integer #f)`.
Defaults to base 10 however it accepts between 2 and 36
`(or Integer Boolean) <- Integer (Optional Integer 10)`

```
> (byte-ascii-digit 98 36)
11
```

```
> (byte-ascii-digit 72)
#f
```

`char-ascii-digit` returns `(or Char #f)`.
Defaults to base 10 however it accepts between 2 and 36
`(or Integer Boolean) <- Char (Optional Integer 10)`

```
> (char-ascii-digit #\a 36)
10
```

```
> (char-ascii-digit #\a)   
#f
```

`char-port-eof?` returns `Boolean`.
`Boolean <- Port`

```
> (def p (open-input-string "(a . (b . (c . ()))) 34"))
> (read p)
(a b c)
> (read p)
34
> (char-port-eof? p)                                   
#t
```

```
> (char-port-eof? (current-input-port))
#f  
```

`byte-port-eof?` returns `Boolean`.
`Boolean <- Port`

```
> (def b (open-input-bytes #u8(112 111)))
> (read b)
po
> (byte-port-eof? b)                                   
#t
```

```
> (char-port-eof? (current-input-port))
#f  
```

`bytes-every` returns `Boolean`.
`Boolean <- Function(pred) Bytes(byte array)`

```
> (def (below-space? b) (< b #x20))
> (bytes-every below-space? #u8(#x09 #x0A #x0C))
#t
```

```
> (def (below-space? b) (< b #x20))
> (bytes-every below-space? #u8(#x29 #x0A #x0C))
#f
```

`bytes-ascii-printable?` returns `Boolean`.
`Boolean <- Bytes(byte array)`

```
> (bytes-ascii-printable? #u8(#x20 #x09 #x0A #x0C))
#t
```

```
> (bytes-ascii-printable? #u8(#x1e #x2d #x09 #x0A #x0C))
#f
```

`parse-error!` raises an Exception
`<- Symbol String Any[Zero or More]`


`open-input-string` expects a natural number in decimal on
the current port, returns it
`<- Port (Optional Integer 10)`

```
> (def mport (open-input-string "2340.99"))
> (expect-natural mport) 
2340
```

```
> (def bport (open-input-string "101011")) 
> (expect-natural bport 2)                 
43
```

`expect-maybe-one-of` takes char-pred functions as input,
however returns a function. The returned function 
takes port as input and returns a character that the first
character that satisfies pred function or `#f`.
`Function <- Function(char-pred?)`

```
> (def (is-uppercase? char) (char<=? #\A char #\Z))
> (def check-for-uppercase  (expect-maybe-one-of is-uppercase?))
> (check-for-uppercase (open-input-string "ABc"))
#\A                
```

```
> (def (is-uppercase? char) (char<=? #\A char #\Z))
> (def check-for-uppercase  (expect-maybe-one-of is-uppercase?))
> (check-for-uppercase (open-input-string "abc"))
#f                
```

`expect-one-of` takes char-pred functions as input,
however returns a function. The returned function 
takes port as input and returns a character that the first
character that satisfies pred function or throws Exception.
`Function <- Function(char-pred?)`

```
> (def (is-uppercase? char) (char<=? #\A char #\Z))
> (def check-for-uppercase (expect-one-of is-uppercase?))
> (check-for-uppercase (open-input-string "ABc"))
#\A                
```

`expect-any-number-of` takes char-pred functions as input,
however returns a function. The returned function 
takes port as input and returns first consecutive characters that
satisfy pred function, otherwise `#f`.
`Function <- Function(char-pred?)`

```
> (def (is-uppercase? char) (char<=? #\A char #\Z))
> (def check-for-uppercase (expect-any-number-of is-uppercase?))
> (check-for-uppercase (open-input-string "ABDca"))
"ABD"                
```

```
> (def (is-uppercase? char) (char<=? #\A char #\Z))
> (def check-for-uppercase (expect-any-number-of is-uppercase?))
> (check-for-uppercase (open-input-string "caBG"))
#f              
```

`expect-one-or-more-of` takes char-pred functions as input,
however returns a function. The returned function 
takes port as input and returns first consecutive characters that
satisfy pred function, otherwise throw Exception.
`Function <- Function(char-pred?)`

```
> (def (is-uppercase? char) (char<=? #\A char #\Z))
> (def check-for-uppercase (expect-any-number-of is-uppercase?))
> (check-for-uppercase (open-input-string "ABDca"))
"ABD"                
```

`expect-maybe-char` takes a character,
however returns a function. The returned function 
takes port as input and returns input character(if there is a match), otherwise `#f`.
`Function <- Char`

```
> (def check-for-uppercase (expect-maybe-char #\E))
> (check-for-uppercase (open-input-string "EWD"))
#\E               
```

```
> (def check-for-uppercase (expect-maybe-char #\E))
> (check-for-uppercase (open-input-string "WD"))
#f
```


`expect-char` takes a character,
however returns a function. The returned function 
takes port as input and returns input character(if there is a match), otherwise throws Exception.
`Function <- Char`

```
> (def check-for-uppercase (expect-char #\E))
> (check-for-uppercase (open-input-string "EWD"))
#\E               
```

`expect-char` takes port as input and performs
left trim.
` <- Port`

```
> (def mport (open-input-string "    EAD"))
> (expect-and-skip-any-whitespace mport)
> (read-char mport) 
#\E               
```

`expect-eof` takes port as input and returns
`#!eof` if the port returns `#!eof` when `read-char`,
otherwise throws Exception.
`(or #!eof Exception) <- Port`

```
> (def mport (open-input-string ""))
> (expect-eof mport)
#!eof
```

`eol-char?` takes character as input and returns
Boolean. However, it would return `#t` only for 
`#\newline` and `#\return`.
`Boolean <- Char`

```
> (eol-char? #\newline)
#t
> (eol-char? #\return) 
#t
> (eol-char? #\r)     
#f
```

`expect-eol` takes port as input and returns
`#\newline` if the port starts with `#\return` and follows
by `#\newline` , otherwise throws Exception.
`(Or #\newline Exception) <- Port`

```
> (def mport (open-input-string "\r\n"))
> (expect-eol mport)
#\newline
```

`expect-literal-string` takes string as input and returns a function,
which would take a port. It string matches first consecutive characters
of port, port would advance by that, otherwise throws Exception.
`Function <- String`

```
> (def mstring "gerbil")
> (def rtn-fn (expect-literal-string mstring))
> (def mport (open-input-string "gerbil"))
> (rtn-fn mport)
> (read-char mport)
#!eof
```

`expect-n-digits` takes integer that determines the number of digits,
however returns a function,which would take a port. 
It string matches first consecutive characters
of port and extract `n` number of digits from it. 
It throws if the number of digits is integer input.
`Function <- Integer (Optional Integer 10)`

```
> (def rtn-fn (expect-n-digits 5))
> (def mport (open-input-string "9087777"))
> (rtn-fn mport)
90877
```

`expect-line` takes a port. And read until new line like read-line.
However, it handles any of the CRLF, CR and LF line endings
`String <- Port`

```
> (def mport 
    (open-input-string "This is the day of joy\n Let us rejoice and merry. \r\n It is August"))
> (expect-line mport)
"This is the day of joy"
> (expect-line mport)
" Let us rejoice and merry. "
> (expect-line mport)
" It is August"
```

`expect-then-eof` takes a function whose only input is a port.
`expect-then-eof` returns a function that accepts port
`String <- Function`

```
> (def mstring "gerbil") 
> (def rtn-fn (expect-literal-string mstring))
> (def mport (open-input-string "gerbil"))
> (def expect-rtn-fn (expect-then-eof rtn-fn))
> (expect-rtn-fn mport)
> (read-char mport)
#!eof 
```

`parse-file` takes a file and a function.
It throws when there is no match
`<- String File (Optional Boolean #f)`

```
;; jo.txt file contain only "gerbil "
;; There should a single space "gerbil"
> (def mstring "gerbil\n")
> (def rtn-fn (expect-literal-string mstring))
> (parse-file "jo.txt" rtn-fn) 
```

`parse-string` takes a string and a function.
It throws when there is no match
`<- String Function (Optional Boolean #f)`

```
> (def mstring "gerbil")
> (def rtn-fn (expect-literal-string mstring))
> (parse-string "gerbil" rtn-fn)
```