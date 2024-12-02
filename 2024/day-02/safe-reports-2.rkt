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
  (if (or (null? num-list)
          (null? (rest num-list)))
      ;; No more number pairs
      '()
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

;; Generate a list of alternative reports created
;; by omitting one of the elements of the report.
;; (generative-alternatives '(1 3 2 4 5)) =>
;; '((3 2 4 5) (1 2 4 5) (1 3 4 5) (1 3 2 5) (1 3 2 4))
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

;; A replacement for `safe-report?` that returns true
;; if the report itself is safe or any of its alternatives
;; is safe. `ormap` short circuts as soon as one
;; safe report is found.
;;
(define (safe-or-alternative? report)
  (ormap safe-report?
         (cons report
               (generate-alternatives report))))

;; Input file names is passed as first argument
(define input-file-name
  (vector-ref (current-command-line-arguments) 0))

(define report-rows
  (read-space-delimited-file input-file-name))

(define number-of-safe-reports
  ; Count the number of safe rows 
  (count identity (map safe-or-alternative? report-rows)))

(printf "~a\n" number-of-safe-reports)
