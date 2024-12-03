#lang racket

;; https://adventofcode.com/2024/day/2

;; Read a space-delimited set of numbers into
;; a list of lists.
;;
(define (read-space-delimited-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (for/list ([line (in-lines)])
        (map string->number (string-split line))))))

;; Take a list of numbers and return the difference
;; between each pair of adjacent numbers.
;;
(define (pairwise-differences num-list)
  (if (or (null? num-list)          ; Even-number case
          (null? (rest num-list)))  ; Odd-number case
      ;; Base case: no more number pairs
      '()
      ;; Inductive case: add the difference to the result
      ;; of the calculation on a list without first element.
      (cons (- (first num-list) (second num-list))
            (pairwise-differences (rest num-list)))))

;; Evaluate the rules for a safe report.
;;
(define (safe-report? report)
  (let* ((diffs (pairwise-differences report))
         (abs-diffs (map abs diffs)))
    (and (or (andmap positive? diffs)    ; All increasing
             (andmap negative? diffs))   ; All decreasing
         (>= (apply min abs-diffs) 1)    ; Differ by at least 1 and
         (<= (apply max abs-diffs) 3)))) ; at most 3

;; Input file name is passed as first argument
(define input-file-name
  (vector-ref (current-command-line-arguments) 0))

(define report-rows
  (read-space-delimited-file input-file-name))

(define number-of-safe-reports
  ; Count the number of safe rows 
  (count identity (map safe-report? report-rows)))

(printf "~a\n" number-of-safe-reports)

