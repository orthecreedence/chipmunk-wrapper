(in-package :chipmunk-wrapper)

(defclass arbiter ()
  ((c :accessor arbiter-c :initarg :c :initform nil)
   (elasticity :accessor arbiter-elasticity :initarg :elasticity :initform nil)
   (friction :accessor arbiter-friction :initarg :friction :initform nil)
   (surface-velocity :accessor arbiter-surface-velocity :initarg :surface-velocity :initform nil)
   (contact-count :accessor arbiter-contact-count :initarg :contact-count :initform 0)
   (normals :accessor arbiter-normals :initarg :normals :initform nil)
   (points :accessor arbiter-points :initarg :points :initform nil)
   (first-contact-p :accessor arbiter-first-contact-p :initarg :first-contact-p :initform nil)
   (ignore-collision :accessor arbiter-ignore-collision :initform nil))
  (:documentation "Stores information about an arbiter object"))

(defun make-arbiter (arbiter-c)
  "Given a C arbiter object, populate an arbiter class and return it."
  (let ((elasticity (cp-a:arbiter-e arbiter-c))
        (friction (cp-a:arbiter-u arbiter-c))
        (surface-velocity (list (cp-a:arbiter-surface_vr-x arbiter-c)
                                (cp-a:arbiter-surface_vr-y arbiter-c)))
        (contact-count (cp:arbiter-get-count arbiter-c))
        (normals nil)
        (points nil)
        (first-contact (not (zerop (cp:arbiter-is-first-contact arbiter-c)))))
    (loop for i from 0 to (1- contact-count) do
      (push (cpw:get-arbiter-normal arbiter-c i) normals)
      (push (cpw:get-arbiter-point arbiter-c i) points))
    (make-instance 'arbiter
                   :c arbiter-c
                   :elasticity elasticity
                   :friction friction
                   :surface-velocity surface-velocity
                   :contact-count contact-count
                   :normals (reverse normals)
                   :points (reverse points)
                   :first-contact-p first-contact)))

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

(defun sync-arbiter-to-c (arbiter)
  "Given an arbiter (lisp) object, sync the elasticity/friction/surface vel to
   the internal arbiter c object."
  (let ((arbiter-c (arbiter-c arbiter))
        (elasticity (arbiter-elasticity arbiter))
        (friction (arbiter-friction arbiter))
        (surface-velocity (arbiter-surface-velocity arbiter)))
    (setf (cp-a:arbiter-e arbiter-c) (coerce elasticity 'double-float)
          (cp-a:arbiter-u arbiter-c) (coerce friction 'double-float)
          (cp-a:arbiter-surface_vr-x arbiter-c) (coerce (car surface-velocity) 'double-float)
          (cp-a:arbiter-surface_vr-y arbiter-c) (coerce (cadr surface-velocity) 'double-float))))

