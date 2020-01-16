#!/bin/bash
SCRIPT="./ex4.30.scm"

function run {
    EXP=$1
    echo "$EXP"
    echo -n "$EXP" | gosh $SCRIPT --
}

run "(define (p1 x) (set! x (cons x '(2))) x) (p1 1) quit"
run "(define (p2 x) (define (p e) e x) (p (set! x (cons x '(2))))) (p2 1) quit"

