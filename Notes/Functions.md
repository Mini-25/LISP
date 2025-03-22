# Lisp Functions Guide

## 1. What is a Function?

A function is a block of code that performs a specific task. In Lisp, functions are first-class citizens, meaning they can be:
* Defined and named.
* Passed as arguments to other functions.
* Returned as values from other functions.

## 2. Defining Functions

In Lisp, you define functions using the `defun` macro.

### Syntax
```lisp
(defun function-name (parameter1 parameter2 ...)
  "Optional documentation string"
  (function-body))
```

* `function-name`: The name of the function.
* `parameters`: The inputs to the function (optional).
* `documentation string`: A description of the function (optional).
* `function-body`: The code that executes when the function is called.

### Example: A Simple Function
```lisp
(defun square (x)
  "Returns the square of x"
  (* x x))
```

* This function takes one parameter `x` and returns its square.

## 3. Calling Functions

To call a function, use its name followed by the arguments in parentheses.

### Example
```lisp
(square 5)   ; returns 25
```

## 4. Multiple Parameters

Functions can take multiple parameters.

### Example
```lisp
(defun add (a b)
  "Returns the sum of a and b"
  (+ a b))

(add 3 4)   ; returns 7
```

## 5. Optional Parameters

You can define optional parameters using `&optional`.

### Example
```lisp
(defun greet (name &optional greeting)
  (if greeting
      (format t "~a, ~a!~%" greeting name)
      (format t "Hello, ~a!~%" name)))

(greet "Alice")                ; prints "Hello, Alice!"
(greet "Bob" "Good morning")   ; prints "Good morning, Bob!"
```

## 6. Returning Values

In Lisp, the last evaluated expression in a function is automatically returned.

### Example
```lisp
(defun add-and-square (a b)
  (square (+ a b)))

(add-and-square 2 3)   ; returns 25
```

## 7. Lambda Functions

Lambda functions are anonymous functions (functions without a name). They are useful for short, one-time-use functions.

### Syntax
```lisp
(lambda (parameters) (function-body))
```

### Example
```lisp
((lambda (x) (* x x)) 5)   ; returns 25
```

## 8. Higher-Order Functions

Lisp supports higher-order functions, which are functions that take other functions as arguments or return them.

### Example: `mapcar`

`mapcar` applies a function to each element of a list and returns a new list.

```lisp
(mapcar #'square '(1 2 3 4))   ; returns (1 4 9 16)
```

* `#'` is shorthand for `function`, which tells Lisp to treat `square` as a function.

## 9. Recursive Functions

Lisp is well-suited for recursion, where a function calls itself.

### Example: Factorial
```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)   ; returns 120
```

## 10. Example Program

Let's write a program that uses multiple functions:

```lisp
(defun area-of-circle (radius)
  "Calculates the area of a circle"
  (* pi (* radius radius)))

(defun print-area (radius)
  (format t "The area of a circle with radius ~a is ~a.~%" radius (area-of-circle radius)))

(print-area 5)
```

Output:
```
The area of a circle with radius 5 is 78.53981633974483.
```

## 11. Function Composition

Function composition is a powerful technique in Lisp where the output of one function serves as the input to another.

### Example
```lisp
(defun double (x)
  (* x 2))

(defun increment (x)
  (+ x 1))

;; Using composition
(defun double-then-increment (x)
  (increment (double x)))

(double-then-increment 5)  ; returns 11
```

## 12. Local Functions with `labels` and `flet`

Lisp allows you to define local functions using `labels` (for recursive local functions) and `flet` (for non-recursive local functions).

### Example with `flet`
```lisp
(defun process-list (list)
  (flet ((double-it (x) (* x 2)))
    (mapcar #'double-it list)))

(process-list '(1 2 3 4))  ; returns (2 4 6 8)
```

### Example with `labels`
```lisp
(defun sum-tree (tree)
  (labels ((sum-node (node)
             (if (listp node)
                 (reduce #'+ (mapcar #'sum-node node))
                 node)))
    (sum-node tree)))

(sum-tree '(1 (2 3) (4 (5 6))))  ; returns 21
```

Remember that Lisp's functional programming approach encourages writing clear, concise, and composable functions that can be combined to solve complex problems elegantly.