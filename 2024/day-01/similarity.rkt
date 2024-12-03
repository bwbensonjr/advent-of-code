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
  (if (or (null? lst)
          (null? (first lst)))
      '()
      (cons (map first lst)
            (transpose (map rest lst)))))

;; Make a hash table mapping each value to the count of
;; that value occuring in the `values` list.
;;
(define (value-counts values)
  (let ((counts (make-hash)))
    (for-each (lambda (v)
                (hash-set! counts
                           v
                           (+ (hash-ref counts v 0) 1)))
              values)
    counts))

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

;; Use the provided algorithm to multiple each occurrence
;; of a number in `list-1` and multiple by the number of
;; occurrences of that number in `list-2`.
;;
(define (similarity-score list-1 list-2)
  (let ((counts (value-counts list-2)))
    (apply +
           (map (lambda (v)
                  (* v (hash-ref counts v 0)))
                list-1))))

(define sim-score (similarity-score (first location-lists)
                                    (second location-lists)))

(printf "~a\n" sim-score)

