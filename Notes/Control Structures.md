# Control Structures in Lisp

## 1. Conditionals

Conditionals allow you to execute code based on whether a condition is true or false.

### `if`

The `if` construct is the simplest conditional. It has the following syntax:

```lisp
(if condition
    true-expression
    false-expression)
```

* If `condition` evaluates to true (`t`), `true-expression` is executed.
* If `condition` evaluates to false (`nil`), `false-expression` is executed.

#### Example

```lisp
(defun check-number (x)
  (if (> x 10)
      (print "x is greater than 10")
      (print "x is 10 or less")))

(check-number 15)   ; prints "x is greater than 10"
(check-number 5)    ; prints "x is 10 or less"
```

### `cond`

The `cond` construct is used for multiple conditions. It's like a series of `if-else` statements.

#### Syntax

```lisp
(cond
  (condition1 result1)
  (condition2 result2)
  ...
  (t default-result))
```

* `cond` evaluates each condition in order. The first condition that evaluates to true (`t`) executes its corresponding result.
* The `t` at the end acts as a default case (like `else`).

#### Example

```lisp
(defun check-grade (score)
  (cond
    ((>= score 90) "A")
    ((>= score 80) "B")
    ((>= score 70) "C")
    (t "F")))

(check-grade 85)   ; returns "B"
(check-grade 60)   ; returns "F"
```

### `when` and `unless`

* `when` executes its body if the condition is true.
* `unless` executes its body if the condition is false.

#### Example

```lisp
(when (> 5 3)
  (print "5 is greater than 3"))   ; prints "5 is greater than 3"

(unless (< 5 3)
  (print "5 is not less than 3"))   ; prints "5 is not less than 3"
```

## 2. Loops

Loops allow you to repeat a block of code multiple times.

### `loop`

The `loop` construct is a powerful and flexible way to create loops.

#### Syntax

```lisp
(loop
  (body))
```

#### Example: Simple Loop

```lisp
(loop for i from 1 to 5
      do (print i))
```

Output:
```
1
2
3
4
5
```

### `dotimes`

The `dotimes` construct is used to iterate a fixed number of times.

#### Syntax

```lisp
(dotimes (variable limit)
  (body))
```

#### Example

```lisp
(dotimes (i 3)
  (print i))
```

Output:
```
0
1
2
```

### `dolist`

The `dolist` construct is used to iterate over a list.

#### Syntax

```lisp
(dolist (variable list)
  (body))
```

#### Example

```lisp
(dolist (x '(1 2 3))
  (print x))
```

Output:
```
1
2
3
```

## 3. Example Program

Let's write a program that uses conditionals and loops:

```lisp
(defun print-multiplication-table (n)
  (dotimes (i 10)
    (format t "~a * ~a = ~a~%" n (+ i 1) (* n (+ i 1)))))

(print-multiplication-table 5)
```

Output:
```
5 * 1 = 5
5 * 2 = 10
5 * 3 = 15
5 * 4 = 20
5 * 5 = 25
5 * 6 = 30
5 * 7 = 35
5 * 8 = 40
5 * 9 = 45
5 * 10 = 50
```

## 4. Advanced Control Structures

### `case`

The `case` construct is similar to a switch statement in other languages. It matches a key against a series of patterns.

#### Syntax

```lisp
(case key
  (pattern1 result1)
  (pattern2 result2)
  ...
  (otherwise default-result))
```

#### Example

```lisp
(defun day-type (day)
  (case day
    ((monday tuesday wednesday thursday friday) 'weekday)
    ((saturday sunday) 'weekend)
    (otherwise 'invalid-day)))

(day-type 'wednesday)  ; returns WEEKDAY
(day-type 'sunday)     ; returns WEEKEND
(day-type 'holiday)    ; returns INVALID-DAY
```

### `progn`

The `progn` special form allows you to execute multiple expressions in sequence and return the value of the last one.

#### Example

```lisp
(progn
  (print "This is the first statement")
  (print "This is the second statement")
  (+ 2 3))  ; returns 5
```

### `do` and `do*`

The `do` and `do*` macros provide a general iteration construct with explicit exit conditions.

#### Syntax

```lisp
(do ((var1 init1 step1)
     (var2 init2 step2)
     ...)
    (end-test result)
  body)
```

#### Example

```lisp
(do ((i 0 (+ i 1))
     (j 10 (- j 1)))
    ((> i j) (format t "Done!"))
  (format t "i = ~a, j = ~a~%" i j))
```

Output:
```
i = 0, j = 10
i = 1, j = 9
i = 2, j = 8
i = 3, j = 7
i = 4, j = 6
i = 5, j = 5
i = 6, j = 4
Done!
```

### Error Handling with `handler-case`

Lisp provides exception handling mechanisms similar to try/catch in other languages.

#### Example

```lisp
(handler-case
    (/ 10 0)  ; Division by zero
  (division-by-zero (c)
    (format t "Caught division by zero: ~a~%" c)
    :error))
```

Output:
```
Caught division by zero: arithmetic error DIVISION-BY-ZERO signalled
:ERROR
```