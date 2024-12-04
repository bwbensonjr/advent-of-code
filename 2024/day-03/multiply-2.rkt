#lang racket

;; https://adventofcode.com/2024/day/3

;; Read a file as a list of strings.
;;
(define (read-lines file-name)
  (with-input-from-file file-name
    (lambda ()
      (for/list ([line (in-lines)])
        line))))

;; Parse a string for valide `mul`, `dont't`,
;; and `do` operations and return them as a
;; list of strings.
;;
(define (parse-ops str)
  (regexp-match* #rx"(mul\\([0-9]+,[0-9]+\\)|don't\\(\\)|do\\(\\))" str))

;; Filter the list of operations for valid `mul()` ops
;; by disabling ops when seeing `dont't` and enabling
;; when seeing `do()`.
;;
(define (enabled-ops op-strings)
  (define (find-enabled-ops op-strs enabled?)
    (if (null? op-strs)
        '()
        (let ((op-str (first op-strs)))
          (cond
           ((string=? op-str "don't()")
            (find-enabled-ops (rest op-strs) #f))
           ((string=? op-str "do()")
            (find-enabled-ops (rest op-strs) #t))
           (enabled? (cons op-str
                           (find-enabled-ops
                            (rest op-strs)
                            #t)))
           (else (find-enabled-ops (rest op-strs) #f))))))
  ;; Start off enabled
  (find-enabled-ops op-strings #t))

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
                (enabled-ops
                 (append-map parse-ops
                             (read-lines input-file-name))))))

;; Input file name is passed as first argument
(define input-file-name
  (vector-ref (current-command-line-arguments) 0))

(define result (parse-multiply-add input-file-name))
(printf "~a\n" result)


