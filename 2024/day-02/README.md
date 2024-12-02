# Advent of Code - Day 2 - 2024

See puzzle: [https://adventofcode.com/2024/day/2](https://adventofcode.com/2024/day/2)

* [`Makefile`](Makefile) - Run scripts in practice and full mode.
* [`data-input.txt`](data-input.txt) - Full data set.
* [`data-practice.txt`](data-practice.txt) - Practice data set.
* [`safe-reports.rkt`](safe-reports.rkt) - Solution to first part of challenge.
* [`safe-reports-2.rkt`](safe-reports-2.rkt) - Solution to second part of challenge.

For the second puzzle I started using the
[Racket](https://www.racket-lang.org/) dialect of the
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
programming language.

I first learned to program in Scheme/Lisp in an AI programming course
using what was to become my favorite Computer Science book [The
Structure and Interpretation of Computer
Programs](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html)
by Abelson and Sussman. One of the very common functional language
idioms that I learned in SICP is the use of recursion and induction as
a natural way to structure data and the algorithms that process that
data. The key to mathematical induction is to define a base case and a
then a transition between a larger case and smaller case, moving
towards the base case.

An example of an inductive/recursive function from the Day 2 challange
is the calculation of the difference between pairs of adjacent numbers
from a number sequence. 

```
;; Take a list of numbers and return the difference
;; between each pair of adjacent numbers.
;;
(define (pairwise-differences num-list)
  (if (or (null? num-list)          ; Even-number case
          (null? (rest num-list)))  ; Odd-number case
      ;; No more number pairs
      '()
      (cons (- (first num-list) (second num-list))
            (pairwise-differences (rest num-list)))))
```

The base case is when the input is either an empty list or list with a
single element, in which case there are no pairs left to process and an
empty list is returned. Otherwise, the transition step calculates
the difference between the first two elements of the input and
adds the answer to the front of the result of calling the function
on the rest of the list, minus the first element.

Tracing this function shows how it destructures the input list and
then builds a new list of the pairwise differences as the result.

```
> (trace pairwise-differences)
> (pairwise-differences '(7 6 4 2 1))
>(pairwise-differences '(7 6 4 2 1))
> (pairwise-differences '(6 4 2 1))
> >(pairwise-differences '(4 2 1))
> > (pairwise-differences '(2 1))
> > >(pairwise-differences '(1))
< < <'()
< < '(1)
< <'(2 1)
< '(2 2 1)
<'(1 2 2 1)
'(1 2 2 1)
```

A slightly more complicated example from the second part of the puzzle
is this function for generating alternative versions of a list leaving
out one of the elements.

```
;; Generate a list of alternative reports created
;; by omitting one of the elements of the report.
;;
(define (generate-alternatives report)
  (define (alternatives rep-head leave-out rep-tail)
    (if (null? rep-tail)
        (list rep-head)
        (cons (append rep-head rep-tail)
              (alternatives (append rep-head (list leave-out))
                            (first rep-tail)
                            (rest rep-tail)))))
  (alternatives '() (first report) (rest report)))
```

This function uses an internally defined function that maintains three
pieces of the calculation as parameters: `rep-head` - the first part
or head of the alternative list, `leave-out` - the element to leave
out, and `rep-tail` - the tail of the alternative list.

```
> (generate-alternatives '(1 3 2 4 5))
>(alternatives '() 1 '(3 2 4 5))
> (alternatives '(1) 3 '(2 4 5))
> >(alternatives '(1 3) 2 '(4 5))
> > (alternatives '(1 3 2) 4 '(5))
> > >(alternatives '(1 3 2 4) 5 '())
< < <'((1 3 2 4))
< < '((1 3 2 5) (1 3 2 4))
< <'((1 3 4 5) (1 3 2 5) (1 3 2 4))
< '((1 2 4 5) (1 3 4 5) (1 3 2 5) (1 3 2 4))
<'((3 2 4 5) (1 2 4 5) (1 3 4 5) (1 3 2 5) (1 3 2 4))
'((3 2 4 5) (1 2 4 5) (1 3 4 5) (1 3 2 5) (1 3 2 4))
```

