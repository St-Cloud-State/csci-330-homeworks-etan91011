;; Insertion Sort Implementation in Lisp with Random Number Generation

;; Seed the random number generator to ensure different results on each execution
(setf *random-state* (make-random-state t))

;; Function to generate a list of random integers
(defun generate-random-list (n)
  "Generates a list of N random integers between 1 and 100."
  (if (<= n 0)  ;; Base case: If N is 0 or negative, return an empty list
      nil
      ;; Otherwise, generate a random number between 1 and 100 and recursively build the list
      (cons (+ 1 (random 100)) (generate-random-list (- n 1)))))

;; Function to insert an element into a sorted list while maintaining order
(defun insert (elem lst)
  "Inserts an element into a sorted list in ascending order."
  (if (or (null lst) (<= elem (car lst))) ;; If list is empty or element is smaller than the first item
      (cons elem lst)  ;; Insert the element at the front
      ;; Otherwise, keep the first item and insert the element recursively into the rest of the list
      (cons (car lst) (insert elem (cdr lst)))))

;; Function to perform Insertion Sort on a list
(defun insertion-sort (lst)
  "Sorts a list using the Insertion Sort algorithm."
  (if (null lst)  ;; Base case: If the list is empty, return an empty list
      nil
      ;; Recursively sort the tail of the list and insert the head in sorted order
      (insert (car lst) (insertion-sort (cdr lst)))))

;; Generate a random list and sort it
(let ((random-list (generate-random-list 10)))  ;; Create a list of 10 random integers
  (format t "Unsorted List: ~a~%" random-list)  ;; Print the unsorted list
  (format t "Sorted List: ~a~%" (insertion-sort random-list)))  ;; Print the sorted list