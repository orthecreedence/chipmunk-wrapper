(in-package :chipmunk-wrapper)

;; defines a very simple class for tracking whether or not a c pointer/class is
;; still active. once destroyed, it is not allowed to be destroyed again.
(defclass base ()
  ((c :accessor base-c :initarg :c)
   (type :accessor base-type :initform :base)
   (active :accessor base-active :initform t)))

(defmethod destroy ((obj base))
  "Doesn't really do anything but track internal state of objects. The point is
  to call this directly before a C physics object is destroyed so it can be 
  tracked whether or not it's active and possibly save exceptions in the 
  foreign library."
  ;; make sure we only free real types
  (assert (find (base-type obj) '(:shape :body :joint :space)))
  (when (base-active obj)
    ;; destroy the object in c-land
    (case (base-type obj)
      (:shape (cp:shape-destroy (base-c obj)))
      (:body (cp:body-destroy (base-c obj)))
      (:joint (cp:constraint-destroy (base-c obj)))
      (:space (cp:space-destroy (base-c obj))))
    ;; mark as inactive so we don't accidentally free it again later
    (setf (base-active obj) nil)
    t))

