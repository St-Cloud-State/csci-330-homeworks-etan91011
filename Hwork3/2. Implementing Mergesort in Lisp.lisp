;; Mergesort Implementation in Lisp (Declarative Style) with True Random Numbers

;; Seed the random number generator to ensure different results each time
(setf *random-state* (make-random-state t))

;; ------------------------------
;; PARTITION FUNCTION
;; ------------------------------
(defun partition (lst)
  "Splits the list into two nearly equal halves using a recursive approach."
  ;; Base case: If the list is empty or has only one element, return it as the first half, with the second half as nil.
  (if (or (null lst) (null (cdr lst)))
      (list lst nil)
      ;; Recursive case: Take the first two elements and distribute them into two halves.
      (let* ((first (car lst))       ;; Take the first element
             (second (cadr lst))      ;; Take the second element (if it exists)
             (rest (cddr lst))        ;; Take the remaining elements
             (split (partition rest))) ;; Recursively partition the rest of the list
        ;; Construct the two halves by distributing elements alternately
        (list (cons first (car split)) (cons second (cadr split))))))

;; ------------------------------
;; MERGE FUNCTION
;; ------------------------------
(defun merge-lists (lst1 lst2)
  "Merges two sorted lists into one sorted list in ascending order."
  (cond
    ;; If the first list is empty, return the second list
    ((null lst1) lst2)
    ;; If the second list is empty, return the first list
    ((null lst2) lst1)
    ;; Compare the first elements of both lists and recursively merge the rest
    ((<= (car lst1) (car lst2))
     (cons (car lst1) (merge-lists (cdr lst1) lst2)))
    ;; Otherwise, place the smaller element first and merge recursively
    (t
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

;; ------------------------------
;; MERGESORT FUNCTION
;; ------------------------------
(defun mergesort (lst)
  "Sorts a list using the Mergesort algorithm, which follows a divide-and-conquer strategy."
  ;; Base case: A list with 0 or 1 elements is already sorted
  (if (or (null lst) (null (cdr lst)))
      lst
      ;; Recursive case: Split the list into two halves and sort each half
      (let* ((split (partition lst))   ;; Partition the list into two halves
             (left (car split))         ;; Extract the left half
             (right (cadr split)))      ;; Extract the right half
        ;; Recursively sort both halves and merge the sorted halves
        (merge-lists (mergesort left) (mergesort right)))))

;; ------------------------------
;; RANDOM LIST GENERATION
;; ------------------------------
(defun generate-random-list (n)
  "Generates a list of N random integers between 1 and 100."
  (if (<= n 0)
      nil  ;; Base case: If N is 0 or negative, return an empty list
      (cons (+ 1 (random 100))  ;; Generate a random number between 1 and 100
            (generate-random-list (- n 1)))))  ;; Recursively generate the rest of the list

;; ------------------------------
;; MAIN PROGRAM EXECUTION
;; ------------------------------
(let ((random-list (generate-random-list 10)))  ;; Generate a random list of 10 numbers
  ;; Print the unsorted list
  (format t "Unsorted List: ~a~%" random-list)
  ;; Print the sorted list using the Mergesort function
  (format t "Sorted List: ~a~%" (mergesort random-list)))