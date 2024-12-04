#lang racket

;; https://adventofcode.com/2024/day/3

;; Read a file as a list of strings.
;;
(define (read-lines file-name)
  (with-input-from-file file-name
    (lambda ()
      (for/list ([line (in-lines)])
        line))))

;; Parse a string for valide multiply operations
;; and return them as a list of strings.
;;
(define (valid-multiply-ops str)
  (regexp-match* #rx"mul\\([0-9]+,[0-9]+\\)" str))

;; Parse the arguments out of the single multiply
;; operation string and convert them to integers.
;;
(define (parse-integer-args mul-op-str)
  (map string->number
       (rest (regexp-match
              #rx"mul\\(([0-9]+),([0-9]+)\\)"
              mul-op-str))))

;; Parse the integer arguments and multiply
;;
(define (parse-and-multiply mul-op-str)
  (apply * (parse-integer-args mul-op-str)))

(define (parse-multiply-add file-name)
  (apply + (map parse-and-multiply
                (append-map valid-multiply-ops
                            (read-lines input-file-name)))))

;; Input file name is passed as first argument
(define input-file-name
  (vector-ref (current-command-line-arguments) 0))

(define result (parse-multiply-add input-file-name))
(printf "~a\n" result)


