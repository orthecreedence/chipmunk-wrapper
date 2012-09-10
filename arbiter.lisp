(in-package :chipmunk-wrapper)

(defun get-arbiter-normal (arbiter i)
  (assert (and (<= 0 i)
               (< (cp-a:arbiter-num-contacts arbiter) i)))
  (let ((vect (cp-a:contact-n (cffi:mem-aref (cp-a:arbiter-contacts arbiter) 'cp:contact i))))
    (list (cp-a:vect-x vect)
          (cp-a:vect-y vect))))

(defun get-arbiter-position (arbiter i)
  (assert (and (<= 0 i)
               (< (cp-a:arbiter-num-contacts arbiter) i)))
  (let ((vect (cp-a:contact-p (cffi:mem-aref (cp-a:arbiter-contacts arbiter) 'cp:contact i))))
    (list (cp-a:vect-x vect)
          (cp-a:vect-y vect))))

