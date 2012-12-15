(in-package :chipmunk-wrapper)

(defclass joint (base)
  ((c-type :initform :joint)
   (space :accessor joint-space :initarg :space :initform nil)
   (body1 :accessor joint-body1 :initarg :body1 :initform nil)
   (body2 :accessor joint-body2 :initarg :body2 :initform nil)))

(defmethod make-joint ((body1 body) (body2 body) (create-fn function))
  "Create a joint/contraint between two bodies."
  (let ((c-obj (funcall create-fn body1 body2)))
    (let ((joint (make-instance 'joint :c c-obj :body1 body1 :body2 body2)))
      (push joint (body-joints body1))
      (push joint (body-joints body2))
      joint)))

(defmethod detach-joint ((joint joint))
  "Detach a joint from a space and its bodies."
  ;; this will remove the constraint from the space AND the bodies
  (cp:space-remove-constraint (base-c (joint-space joint)) (base-c joint))
  (setf (joint-space joint) nil
        (body-joints (joint-body1 joint)) (remove joint (body-joints (joint-body1 joint)))
        (body-joints (joint-body2 joint)) (remove joint (body-joints (joint-body2 joint)))))

