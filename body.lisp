(in-package :chipmunk-wrapper)

(defparameter *body-registry* (make-hash-table :test #'eql)
  "Tracks all bodies created in a reverse pointer -> lisp obj index. Useful for
  referencing a lisp body only by its c pointer (from, say, a callback).")

(defclass body (base)
  ((c-type :initform :body)
   (shapes :accessor body-shapes :initform nil)
   (x :accessor body-x :initarg :x :initform 0)
   (y :accessor body-y :initarg :y :initform 0)
   (angle :accessor body-angle :initarg :angle :initform 0)))

(defun make-body (create-fn)
  "Create a new body. Body creation can be involved (calculating MOI and mass)
  and as such is done so through the create-fn given. Once created, the body is
  added to body registry, which allows the lisp body object to be referenced by
  its c pointer (useful for callbacks).
  
  Initialization of the body, including setting up its initial position/angle,
  are done through the create-fn callback. The lisp body class will sync its
  values with the c object on every space run automatically."
  (let ((c-obj (funcall create-fn)))
    (assert (cffi:pointerp c-obj))
    (let ((body (make-instance 'body :c c-obj)))
      (setf (gethash c-obj *body-registry*) body)
      body)))

