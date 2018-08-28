# L0

[![Build Status](https://travis-ci.org/eudoxia0/l0.svg?branch=master)](https://travis-ci.org/eudoxia0/l0)

A minimal Lisp with linear types. Not yet finished.

Example:

~~~bash
$ make l0
$ ./l0 examples/fib.lisp fib.cpp
$ g++ fib.cpp
$ ./a.out
fib(30) = 832040
~~~

## Examples

Try running `make test` to compile and run the examples.

### Hello World

~~~lisp
(defun main () i32
  (println "Hello, world!")
  0)
~~~

### Fibonacci

~~~lisp
(defun fib ((n i32)) i32
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
~~~

## License

Copyright 2018 Fernando Borretti.

Licensed under the GPLv3 license. See the COPYING file for details.
