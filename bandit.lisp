;;; 10 arm bandit test bed

;;; Thin Common Lisp wrapper for GNUPlot
;;; https://github.com/ormf/cl-plot

;;; understand this later, from https://gist.github.com/serialhex/318d5ce9d2bbf0c4c98d29a814383e60
(defun normal-random (mean std-dev)
  "Normal random numbers, with the given mean & standard deviation."
  (do* ((rand-u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-s (+ (* rand-u rand-u) (* rand-v rand-v))
                (+ (* rand-u rand-u) (* rand-v rand-v))))
    ((not (or (= 0 rand-s) (>= rand-s 1)))
     (+ mean
      (* std-dev
	 (* rand-u (sqrt (/ (* -2.0 (log rand-s)) rand-s))))))))

(defun generate-action-values (n)
  (loop for i from 0 to n collect (normal-random 0 1)))

(defvar action-values (generate-action-values 10))
(setq action-values (generate-action-values 10))

(defvar Qs (loop for i from 1 to 10 collect 0))
(defvar Ns (loop for i from 1 to 10 collect 0))

(defun bandit (action-values a)
  (normal-random (nth (- a 1) action-values) 1))

(defun argmax (array)
  (let ((max-elem (loop for elem in array maximizing elem)))
    (loop for i from 0 below (length array)
	  do (if (eq max-elem (nth i array))
		 (return i)))))

(defun be-greedy? (epsilon index)
  (if (> (mod index 100) (* epsilon 100))
      t))

(defun initialize-zeros (k) (loop for i from 0 to k collect 0))

(defun mean (l) (/ (reduce #'+ l) (length l)))

(defun k-bandit-algorithm (k epsilon)
  (let ((bandits (generate-action-values k))
	(R_avg 0))
    (defun bandit (a)
      (normal-random (nth a bandits) 1))
    (let ((Qs (initialize-zeros k))
	  (Ns (initialize-zeros k)))
      (loop for i from 1 to 1000
	    do (let ((A (if (be-greedy? epsilon i) (argmax Qs) (random k))))
		 (let ((R (bandit A)))
		   (setf R_avg (/ (+ (if (eq R_avg 0) R R_avg) R) 2))
		   (setf (nth A Ns) (+ (nth A Ns) 1))
		   (setf (nth A Qs) (+ (nth A Qs)
				       (/ (- R (nth A Qs))
					  (nth A Ns))))))))
    R_avg))


(defun random-list (n mean std-dev)
  (loop for i from 1 to n collect (normal-random mean std-dev)))

(defun round-to-tenths (num)
  (float (/ (round (* num  10)) 10)))

(defvar rand-l (sort (random-list 1000 0 1) #'<))

(defun bin-list-by-tenths (l)
  (loop for elem in l collect (round-to-tenths elem)))

(defvar tenths-l (bin-list-by-tenths rand-l))

