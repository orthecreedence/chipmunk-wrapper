(in-package :chipmunk-wrapper)

;; defines a very simple class for tracking whether or not a c pointer/class is
;; still active. once destroyed, it is not allowed to be destroyed again.
(defclass base ()
  ((c :accessor base-c :initarg :c)
   (c-type :accessor base-type :initform :base)
   (active :accessor base-active :initform t)))

(defmethod destroy ((obj base))
  "Call to destroy the c object this physics object wraps and also track whether
  or not it has been destroyed. Since chipmunk has 4 main types, this is a
  fairly simple function."
  ;; make sure we only free real types
  (assert (find (base-type obj) '(:shape :body :joint :space)))
  (when (base-active obj)
    ;; destroy the object in c-land
    (case (base-type obj)
      (:shape
        (cp:shape-destroy (base-c obj)))
      (:body
        (remhash (base-c obj) *body-registry*)
        (cp:body-destroy (base-c obj)))
      (:joint
        (detach-joint obj)
        (cp:constraint-destroy (base-c obj)))
      (:space
        (cp:space-destroy (base-c obj))))
    ;; mark as inactive so we don't accidentally free it again later
    (setf (base-active obj) nil)
    t))

