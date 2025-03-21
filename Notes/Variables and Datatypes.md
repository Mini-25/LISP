# Variables and Data Types

## 1. Variables in Lisp

Variables are used to store values that can be used later in your program. In Lisp, there are two main types of variables:
* **Global Variables**: Accessible throughout the entire program.
* **Local Variables**: Accessible only within a specific scope.

### Global Variables

* Use `setq`, `defvar`, or `defparameter` to define global variables.

#### Using `setq`

```lisp
(setq x 10)   ; x is now 10
```

* `setq` assigns a value to a variable. If the variable doesn't exist, it creates it.

#### Using `defparameter`

```lisp
(defparameter *y* 20 "This is a global variable")
```

* `defparameter` is similar to `setq`, but it's typically used for defining global variables that won't change.
* By convention, global variables are enclosed in `*` (e.g., `*y*`).
* `defparameter` always redefines the variable, even if it already exists.

#### Using `defvar`

```lisp
(defvar *z* 30 "Another global variable")
```

* `defvar` only initializes the variable if it doesn't already exist.
* If the variable already exists, `defvar` keeps the existing value.

### Local Variables

* Use `let` or `let*` to define local variables within a specific scope.

#### Using `let`

```lisp
(let ((a 5)
      (b 10))
  (+ a b))   ; returns 15
```

* The variables `a` and `b` are only accessible inside the `let` block.
* The syntax is `(let ((var1 value1) (var2 value2) ...) body)`.
* In a `let` binding, variables are defined in parallel (none of the bindings can refer to the others).

#### Using `let*`

```lisp
(let* ((a 5)
       (b (* a 2)))  ; b can refer to a
  (+ a b))           ; returns 15
```

* With `let*`, variables are defined sequentially, so later variables can refer to earlier ones.

## 2. Data Types in Lisp

Lisp is dynamically typed, meaning you don't need to declare the type of a variable explicitly. Here are the most common data types:

### Numbers

* Integers and floating-point numbers:

```lisp
42        ; integer
3.14      ; float
1/3       ; rational (in Common Lisp)
#C(2 3)   ; complex number (2+3i)
```

### Strings

* Text enclosed in double quotes:

```lisp
"Hello, Lisp!"
```

* String operations:

```lisp
(length "Hello")           ; returns 5
(string= "abc" "abc")      ; returns T (string equality)
(string< "abc" "def")      ; returns T (string comparison)
(subseq "Hello" 1 3)       ; returns "el" (substring)
```

### Symbols

* Symbols are used as identifiers for variables, functions, etc.:

```lisp
x
my-variable
```

* Symbols can be case-insensitive in many Lisp implementations.

### Lists

* Lists are the most fundamental data structure in Lisp:

```lisp
(1 2 3 4)          ; list of numbers
("a" "b" "c")      ; list of strings
(1 "two" 3.0)      ; mixed list
```

### Arrays and Vectors

* Arrays are fixed-size containers:

```lisp
#(1 2 3 4)         ; vector (one-dimensional array)
```

* Creating arrays:

```lisp
(make-array 5)                 ; creates a vector of length 5
(make-array '(3 3))            ; creates a 3x3 matrix
```

### Booleans

* `t` represents true, and `nil` represents false:

```lisp
t    ; true
nil  ; false
```

* Any value other than `nil` is considered true in a boolean context.

### Hash Tables

* Hash tables provide fast key-value lookups:

```lisp
(defvar *ht* (make-hash-table))        ; create a hash table
(setf (gethash 'key *ht*) 'value)      ; set a key-value pair
(gethash 'key *ht*)                    ; retrieve a value (returns 'value)
```

## 3. Basic Operations

Let's look at some basic operations you can perform with variables and data types.

### Arithmetic Operations

* Addition:

```lisp
(+ 1 2 3)   ; returns 6
```

* Subtraction:

```lisp
(- 10 4)    ; returns 6
(- 10 4 1)  ; returns 5 (10 - 4 - 1)
```

* Multiplication:

```lisp
(* 2 3)     ; returns 6
(* 2 3 4)   ; returns 24
```

* Division:

```lisp
(/ 10 2)    ; returns 5
(/ 10 3)    ; returns 10/3 (a rational in Common Lisp)
(/ 10 3.0)  ; returns 3.3333333
```

* Exponentiation:

```lisp
(expt 2 3)  ; returns 8 (2³)
```

### Comparison Operations

```lisp
(= 5 5)        ; returns T (numeric equality)
(/= 5 6)       ; returns T (numeric inequality)
(< 5 10)       ; returns T (less than)
(<= 5 5)       ; returns T (less than or equal)
(> 10 5)       ; returns T (greater than)
(>= 10 10)     ; returns T (greater than or equal)
```

### Logical Operations

```lisp
(and T T)      ; returns T
(and T nil)    ; returns nil
(or T nil)     ; returns T
(or nil nil)   ; returns nil
(not T)        ; returns nil
(not nil)      ; returns T
```

### String Operations

* Concatenate strings:

```lisp
(concatenate 'string "Hello, " "Lisp!")   ; returns "Hello, Lisp!"
```

### List Operations

* Get the first element of a list:

```lisp
(car '(1 2 3))   ; returns 1
```

* Get the rest of the list (excluding the first element):

```lisp
(cdr '(1 2 3))   ; returns (2 3)
```

* Create a new list:

```lisp
(list 1 2 3)     ; returns (1 2 3)
```

* Add an element to the beginning of a list:

```lisp
(cons 1 '(2 3))  ; returns (1 2 3)
```

* Check if something is a list:

```lisp
(listp '(1 2 3))  ; returns T
(listp 42)        ; returns nil
```

* Get the length of a list:

```lisp
(length '(1 2 3))  ; returns 3
```

## 4. Type Conversion

Lisp provides functions to convert between different data types:

```lisp
(number-to-string 42)          ; returns "42"
(parse-integer "42")           ; returns 42
(intern "SYMBOL")              ; converts a string to a symbol
(symbol-name 'symbol)          ; returns "SYMBOL"
```

## 5. Example Program

Let's write a simple program that uses variables and data types:

```lisp
(defparameter *name* "Alice")   ; global variable

(let ((age 25))                 ; local variable
  (format t "Hello, ~a! You are ~d years old.~%" *name* age))
```

Output:

```
Hello, Alice! You are 25 years old.
```

More complex example using multiple data types:

```lisp
(defparameter *users* '(("Alice" 25) ("Bob" 30) ("Charlie" 22)))

(defun print-user-info (users)
  (dolist (user users)
    (let ((name (car user))
          (age (cadr user)))
      (format t "~a is ~d years old.~%" name age))))

(print-user-info *users*)
```

Output:

```
Alice is 25 years old.
Bob is 30 years old.
Charlie is 22 years old.
```