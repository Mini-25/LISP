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

* Comments in Lisp start with a semicolon `;`.
* Example:

```lisp
; This is a comment
(print "Hello, World!")   ; This is also a comment
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
