#lang racket

;; https://adventofcode.com/2024/day/1

;; Read a space-delimited set of numbers into
;; a list of lists.
;;
(define (read-space-delimited-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (for/list ([line (in-lines)])
        (map string->number (string-split line))))))

;; Transpose the rows and columns of a list of lists.
;;
(define (transpose lst)
  (if (or (null? lst) (null? (car lst)))
      '()
      (cons (map car lst)
            (transpose (map cdr lst)))))

;; The input-file-name should be passed an argument to the script.
;;
(define input-file-name
  (vector-ref (current-command-line-arguments) 0))

;; Read the file as a list of rows and transpose to
;; two lists of columns.
;;
(define location-lists
  (transpose
   (read-space-delimited-file input-file-name)))

;; Compute the magnitude difference of the sorted lists.
;;
(define (compute-differences list-1 list-2)
  (apply +
         (map (lambda (n1 n2) (abs (- n1 n2)))
              (sort list-1 <)
              (sort list-2 <))))

(define difference-total
  (compute-differences (first location-lists)
                       (second location-lists)))

(printf "~a\n" difference-total)
