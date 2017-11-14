# SICP 4.1.3 Evaluator Data Structures

## Operations on Environments

[As explained in section 3.2,] an environment is a sequence of frames,
    where each frame is a table of bindings that associate variables
    with their corresponding values.

We use the following operations for manipulating environments:

* (lookup-variable-value <var> <env>)
  returns the value that is bound to the symbol <var>
  in the environment <env>, or signals an error if the variable is unbound.
* (extend-environment <variables> <values> <base-env>
  returns a new environment, consisting of a new frame in which the symbols in
  the list <variables> are bound to the corresponding elements in the list
  <values>, where the enclosing environment is <base-env>.
* (define-variable! <var> <val> <env>)
  adds to the first frame in the environment <env> a new binding that associates
  the variable <var> with the value <value>
* (set-variable-value! <var> <val> <env>)
  changes the binding of the value <var> in the environment <env>
  so that the variable is now bound to the value <value>, or signals
  an error if the variable is unbound.

## The definition of frames in the textbook

Each frame of an environment is represented as a pair of lists:
     a list of the variables bound in that frame
     and a list of the associated values.

(define my-frame '((a b c) . (1 2 3)))

## Excercise 4.11

Instead of representing a pair of lists,
we can represent a frame as a list of bindings, where each binding
is a name-value pair.

(define my-frame-2 '((a . 1) (b . 2) (c . 3)))

### define-variable!

* only change the first frame of the environment, <env>
* if the variable, <var> is unbound, add the binding, <var> and <val>
  to the first frame.
* otherwise, i.e. <var> is bound, change the bound value to <val>.


