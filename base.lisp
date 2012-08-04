(in-package :chipmunk-wrapper)

;; defines a very simple class for tracking whether or not a c pointer/class is
;; still active. once destroyed, it is not allowed to be destroyed again.
(defclass base ()
  ((c :accessor base-c :initarg :phx)
   (type :accessor base-type :initform :base)
   (active :accessor base-active :initform t)))

(defmethod destroy ((obj base))
  "Doesn't really do anything but track internal state of objects. The point is
  to call this directly before a C physics object is destroyed so it can be 
  tracked whether or not it's active and possibly save exceptions in the 
  foreign library."
  (unless (base-active obj)
    ;(error "Object of type ~A has already been destroyed" (phx-obj-type obj))
    ;(output " - object of type ~a (~a) has already been destroyed.~%" (phx-obj-type obj) obj :level +log-notice+)
    (return-from destroy nil))
  ;(output " - destroying object of type ~a (~a) : ~a~%" (phx-obj-type obj) (if (eql (phx-obj-type obj) :shape) (shape-type obj) "") obj)
  (setf (base-active obj) nil)
  t)

