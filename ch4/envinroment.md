# SICP 4.1.3 Evaluator Data Structures

## Operations on Environments

[As explained in section 3.2,] an environment is a sequence of frames,
    where each frame is a table of bindings that associate variables
    with their corresponding values.

We use the following operations for manipulating environments:

* (lookup-variable-value &lt;var&gt; &lt;env&gt;)
  returns the value that is bound to the symbol &lt;var&gt;
  in the environment &lt;env&gt;, or signals an error if the variable is unbound.
* (extend-environment &lt;variables&gt; &lt;values&gt; &lt;base-env&gt;
  returns a new environment, consisting of a new frame in which the symbols in
  the list &lt;variables&gt; are bound to the corresponding elements in the list
  &lt;values&gt;, where the enclosing environment is &lt;base-env&gt;.
* (define-variable! &lt;var&gt; &lt;val&gt; &lt;env&gt;)
  adds to the first frame in the environment &lt;env&gt; a new binding that associates
  the variable &lt;var&gt; with the value &lt;value&gt;
* (set-variable-value! &lt;var&gt; &lt;val&gt; &lt;env&gt;)
  changes the binding of the value &lt;var&gt; in the environment &lt;env&gt;
  so that the variable is now bound to the value &lt;value&gt;, or signals
  an error if the variable is unbound.

## The definition of frames in the textbook

Each frame of an environment is represented as a pair of lists:
     a list of the variables bound in that frame
     and a list of the associated values.

```
(define my-frame '((a b c) . (1 2 3)))
```

## Excercise 4.11

Instead of representing a pair of lists,
we can represent a frame as a list of bindings, where each binding
is a name-value pair.

```
(define my-frame-2 '((a . 1) (b . 2) (c . 3)))
```

### define-variable!

* only change the first frame of the environment, &lt;env&gt;
* if the variable, &lt;var&gt; is unbound, add the binding, &lt;var&gt; and &lt;val&gt;
  to the first frame.
* otherwise, i.e. &lt;var&gt; is bound, change the bound value to &lt;val&gt;.


