# Hello World in Lisp

There are several ways to output text in Lisp. Here are the common approaches:

## Using `print`

```lisp
(print "Hello, world!")  ;; => "Hello, world!"
```

* `print` outputs the string with double quotes and adds a newline
* It shows the string as a Lisp object (including the quotes)

## Using `princ`

```lisp
(princ "Hello, world!")  ;; => Hello, world!
```

* `princ` outputs the string without quotes
* Intended for human-readable output
* Does not automatically add a newline

## Using `format`

```lisp
(format t "Hello, World")  ;; => Hello, World
```

* `format` is the most versatile output function
* `t` directs output to standard output (terminal)
* Basic usage without format directives doesn't add a newline
* Can add a newline with `~%`: `(format t "Hello, World~%")`

## Using `write-line`

```lisp
(write-line "Hello, World")  ;; => Hello, World
```

* `write-line` outputs the string and adds a newline
* Returns the string that was output

## Using `write-string`

```lisp
(write-string "Hello, World")  ;; => Hello, World
```

* `write-string` outputs the string without adding a newline
* Returns the string that was output

## Using `write`

```lisp
(write "Hello, World")  ;; => "Hello, World"
```

* `write` is a generic output function that prints in a machine-readable way
* By default shows quotes for strings
* Does not add a newline automatically

## Newlines

You can add a newline separately with `terpri`:

```lisp
(terpri)  ;; => NIL (produces a newline)
```

* `terpri` stands for "terminate print line"
* It outputs a newline character and returns NIL
