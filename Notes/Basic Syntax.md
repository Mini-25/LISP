# Basic Syntax

## 1. Atoms and Lists

In Lisp, everything is either an **atom** or a **list**.

### Atoms:
* Atoms are the basic building blocks of Lisp.
* Examples:

```lisp
42          ; number
"hello"     ; string
x           ; symbol
t           ; boolean true
nil         ; boolean false or empty list
```

### Lists:
* Lists are sequences of atoms or other lists, enclosed in parentheses `()`.
* Examples:

```lisp
(1 2 3)               ; list of numbers
(a b c)               ; list of symbols
(1 (2 3) (4 5 6))     ; nested list
```

## 2. S-Expressions

* **S-expressions** (symbolic expressions) are the core of Lisp syntax.
* An S-expression is either an atom or a list.
* Examples:

```lisp
42          ; atom (S-expression)
(+ 1 2 3)   ; list (S-expression)
```

## 3. Function Calls

* In Lisp, function calls are written as lists.
* The first element of the list is the function name, and the rest are the arguments.
* Example:

```lisp
(+ 1 2 3)   ; calls the + function with arguments 1, 2, and 3
```

* This evaluates to `6`.

## 4. Special Forms

* Some expressions in Lisp are **special forms**, which don't follow the standard evaluation rules.
* Examples:
  * `if`: Conditional expression.

```lisp
(if (> x 10)
    (print "x is greater than 10")
    (print "x is 10 or less"))
```

  * `let`: Local variable binding.

```lisp
(let ((x 10)
      (y 20))
  (+ x y))   ; returns 30
```

## 5. Quotes and Quoting

* **Quoting** is used to prevent evaluation of an expression.
* Use the `quote` function or the `'` shorthand.
* Example:

```lisp
(quote (1 2 3))   ; returns the list (1 2 3)
'(1 2 3)          ; same as above, using shorthand
```

* Without quoting, `(1 2 3)` would be interpreted as a function call, which would cause an error.

## 6. Comments

### Single-line Comments
* Comments in Lisp start with a semicolon `;`.
* Example:

```lisp
; This is a comment
(print "Hello, World!")   ; This is also a comment
```

### Multi-line Comments
* For multi-line comments, Common Lisp uses `#|` to start and `|#` to end a comment block.
* Example:

```lisp
#|
This is a
multi-line comment
in Common Lisp
|#
(print "Hello after comment")
```

* In some Lisp dialects like Scheme, multi-line comments use `#;` to comment out an entire expression.
* Example in Scheme:

```scheme
#; (this entire
    s-expression
    is commented out)
(display "Hello")
```

## 7. Evaluation Rules

* Lisp evaluates expressions in the following way:
  1. If it's an atom:
     * Numbers, strings, and `t`/`nil` evaluate to themselves.
     * Symbols evaluate to their value (e.g., a variable's value).
  2. If it's a list:
     * The first element is treated as a function or special form.
     * The rest of the elements are evaluated as arguments.

## 8. Example: Breaking Down a Lisp Expression

Let's break down the expression `(+ (* 2 3) 4)`:

1. `(* 2 3)` is evaluated first:
   * `*` is the multiplication function.
   * `2` and `3` are arguments.
   * Result: `6`.
2. `(+ 6 4)` is evaluated next:
   * `+` is the addition function.
   * `6` and `4` are arguments.
   * Result: `10`.

## 9. Reader Macros

* Reader macros provide shorthand notation in Lisp.
* Common reader macros include:

```lisp
'expr      ; shorthand for (quote expr)
#'expr     ; shorthand for (function expr)
`expr      ; shorthand for backquote (template)
,expr      ; unquote within a backquote
,@expr     ; unquote-splicing within a backquote
#(1 2 3)   ; vector literal
#\c        ; character literal (for the character c)
```

## 10. Package Notation

* Symbols in Lisp belong to packages
* Package notation uses colons:

```lisp
package:symbol    ; external symbol access
package::symbol   ; internal symbol access
:keyword          ; keyword symbol (self-evaluating)
```

## 11. String Syntax

* Strings are enclosed in double quotes
* Special characters are escaped with backslash:

```lisp
"Hello, World!"           ; basic string
"Line 1\nLine 2"          ; newline character
"Tab\tcharacter"          ; tab character
"Quote: \"quoted text\"." ; escaped quotes
```

## 12. Numbers and Math Notation

* Lisp supports various number formats:

```lisp
42        ; integer
3.14      ; floating point
-273      ; negative number
1/3       ; rational number (in Common Lisp)
#b1010    ; binary (= 10)
#o755     ; octal (= 493)
#x1F      ; hexadecimal (= 31)
#C(2 3)   ; complex number (2+3i in Common Lisp)
```