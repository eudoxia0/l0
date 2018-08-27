;; -*- mode: lisp -*-

(defun main () i32
  (let ((tup (tuple 3 4)))
    (println "Hello, world!")
    (print "(proj tup 0) = ")
    (println (proj tup 0))
    (print "(proj tup 1) = ")
    (println (proj tup 1)))
  (let ((tup (tuple 1 2 3)))
    (bind (a b c) tup
      (print "a = ")
      (println a)
      (print "b = ")
      (println b)
      (print "c = ")
      (println c)))
  0)
