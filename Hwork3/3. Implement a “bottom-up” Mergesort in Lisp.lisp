;; Bottom-Up Mergesort Implementation in Lisp with Random Number Generation

;; Seed the random number generator to ensure different results each time
(setf *random-state* (make-random-state t))

;; Function to generate a list of N random integers between 1 and 100
(defun generate-random-list (n)
  "Generates a list of N random integers between 1 and 100."
  (if (<= n 0)
      nil  ;; Base case: If n is 0 or negative, return an empty list.
      (cons (+ 1 (random 100))  ;; Generate a random number between 1 and 100.
            (generate-random-list (- n 1)))))  ;; Recursively generate the rest of the list.

;; Function to partition the input list into sorted pairs
(defun make-pairs (lst)
  "Partitions a list into sorted pairs."
  (cond
    ((null lst) nil)  ;; Base case: If the list is empty, return nil.
    ((null (cdr lst)) (list (list (car lst))))  ;; If there is only one element left, wrap it in a list.
    (t (cons (sort (list (car lst) (cadr lst)) #'<)  ;; Sort the first two elements and create a pair.
             (make-pairs (cddr lst))))))  ;; Recursively process the remaining elements.

;; Function to merge two sorted lists into one sorted list
(defun merge-lists (lst1 lst2)
  "Merges two sorted lists into one sorted list."
  (cond
    ((null lst1) lst2)  ;; If the first list is empty, return the second list.
    ((null lst2) lst1)  ;; If the second list is empty, return the first list.
    ((<= (car lst1) (car lst2))  ;; If the first element of lst1 is smaller or equal,
     (cons (car lst1) (merge-lists (cdr lst1) lst2)))  ;; Add it to the result and continue merging.
    (t  ;; Otherwise, add the first element of lst2 to the result and continue merging.
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

;; Function to merge adjacent lists in a single pass
(defun merge-pass (lst)
  "Merges adjacent lists in a single pass."
  (cond
    ((null lst) nil)  ;; Base case: If the list is empty, return nil.
    ((null (cdr lst)) lst)  ;; If only one list remains, return it as is.
    (t (cons (merge-lists (car lst) (cadr lst))  ;; Merge the first two lists in the pair.
             (merge-pass (cddr lst))))))  ;; Recursively merge the remaining lists.

;; Function to perform bottom-up Mergesort on a list
(defun bottom-up-mergesort (lst)
  "Sorts a list using bottom-up Mergesort."
  (let ((pairs (make-pairs lst)))  ;; Step 1: Partition the list into sorted pairs.
    (loop while (cdr pairs) do  ;; Step 2: Keep merging until only one list remains.
      (setf pairs (merge-pass pairs)))  ;; Perform merging passes.
    (car pairs)))  ;; Return the final sorted list.

;; Generate a random list of 10 numbers and sort it
(let ((random-list (generate-random-list 10)))  ;; Generate a list of 10 random numbers.
  (format t "Unsorted List: ~a~%" random-list)  ;; Print the unsorted list.
  (format t "Sorted List: ~a~%" (bottom-up-mergesort random-list)))  ;; Sort and print the sorted list.