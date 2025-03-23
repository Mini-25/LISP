### Hello World

##### Using `Print`:

```lisp
(print "Hello World")
```

* `print` outputs the string with quotes and a newline.

##### Using `Princ`:
```lisp
(princ "Hello World")
```

* `princ` outputs the string without quotes and a newline.

##### Using `Format`:
```lisp
(format t "Hello World")
```
* `format` outputs the string with a newline.
    * `t` means "print to the standard output."
    * `~%` adds a newline.