(in-package :chipmunk-wrapper)

(defun get-arbiter-normal (arbiter i)
  "Given an arbiter C pointer (ie one passed from a collision callback), grab
   the collision normal as a list '(x y)."
  (assert (and (<= 0 i)
               (< i (cp-a:arbiter-num-contacts arbiter))))
  (let ((contact (cffi:mem-aref (cp-a:arbiter-contacts arbiter) 'cp:contact i)))
    (list (cp-a:contact-n-x contact)
          (cp-a:contact-n-y contact))))

(defun get-arbiter-point (arbiter i)
  "Given an arbiter C pointer (ie one passed from a collision callback), grab
   the collision point as a list '(x y)."
  (assert (and (<= 0 i)
               (< i (cp-a:arbiter-num-contacts arbiter))))
  (let ((contact (cffi:mem-aref (cp-a:arbiter-contacts arbiter) 'cp:contact i)))
    (list (cp-a:contact-p-x contact)
          (cp-a:contact-p-y contact))))
