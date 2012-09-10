(in-package :chipmunk-wrapper)

(defun get-arbiter-normal (arbiter i)
  "Given an arbiter C pointer (ie one passed from a collision callback), grab
   the collision normal as a list '(x y)."
  (assert (and (<= 0 i)
               (< (cp-a:arbiter-num-contacts arbiter) i)))
  (let ((vect (cp-a:contact-n (cffi:mem-aref (cp-a:arbiter-contacts arbiter) 'cp:contact i))))
    (list (cp-a:vect-x vect)
          (cp-a:vect-y vect))))

(defun get-arbiter-point (arbiter i)
  "Given an arbiter C pointer (ie one passed from a collision callback), grab
   the collision point as a list '(x y)."
  (assert (and (<= 0 i)
               (< (cp-a:arbiter-num-contacts arbiter) i)))
  (let ((vect (cp-a:contact-p (cffi:mem-aref (cp-a:arbiter-contacts arbiter) 'cp:contact i))))
    (list (cp-a:vect-x vect)
          (cp-a:vect-y vect))))

