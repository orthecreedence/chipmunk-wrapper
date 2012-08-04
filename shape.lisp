(in-package :chipmunk-wrapper)

(defconstant +shape-types+ '(:circle :segment :poly))

(defclass shape (base)
  ((c-type :initform :shape)
   (type :accessor shape-type :initarg :type :initform nil)
   (body :accessor shape-body :initarg :body :initform nil)))

(defun make-shape (type create-cb &key body)
  (assert (find type +shape-types+))
  (let ((c-obj 

