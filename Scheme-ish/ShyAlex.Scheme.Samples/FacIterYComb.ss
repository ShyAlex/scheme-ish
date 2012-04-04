(define (fac-iter-ycomb f n acc)
  (if (< n 2) acc (f f (- n 1) (* acc n))))
(fac-iter-ycomb fac-iter-ycomb 5 1)