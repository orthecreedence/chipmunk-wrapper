(in-package :chipmunk-wrapper)

(defconstant +dt+ (coerce (/ 1 60) +physics-precision+))

(defclass space (base)
  ((c-type :initform :space)
   (static-body :accessor space-static-body :initarg :static-body :initform nil)
   (timer :accessor space-timer :initform (list :accum 0 :cur-time (get-internal-real-time)))
   (bodies :accessor space-bodies :initform nil)
   (shapes :accessor space-shapes :initform nil)
   (joints :accessor space-joints :initform nil)))

(defun make-space (&key gravity-x gravity-y)
  "Stupid wrapper around space creation."
  (let ((c-obj (cp:space-new)))
    (when gravity-x
      (setf (cp-a:space-gravity-x c-obj) (coerce gravity-x +physics-precision+)))
    (when gravity-y
      (setf (cp-a:space-gravity-y c-obj) (coerce gravity-y +physics-precision+)))
    (let ((space (make-instance 'space
                                :c c-obj
                                :static-body (make-instance 'body :c (cp-a:space-static-body c-obj)))))
      (setf (body-space (space-static-body space)) space)
      space)))

(defmethod space-add-body ((space space) (body body) &key add-all)
  "Add a body to a space. If :add-all is T then all of the body's shapes will
  be added to the space along with the body."
  (when (body-space body)
    (error "This body is already added to a space. Remove it first with space-remove-body"))
  (cp:space-add-body (base-c space) (base-c body))
  (when add-all
    (dolist (shape (body-shapes body))
      (space-add-shape space shape)))
  (push body (space-bodies space))
  (setf (body-space body) space))

(defmethod space-remove-body ((space space) (body body) &key remove-all)
  "Remove a body from a space. If :remove-all is T then all of the body's
  shapes will be removed along with the body."
  (when (eq (body-space body) space)
    (cp:space-remove-body (base-c space) (base-c body))
    (when remove-all
      (dolist (shape (body-shapes body))
        (space-remove-shape space shape)))
    (setf (body-space body) nil)))

(defmethod space-add-shape ((space space) (shape shape))
  "Add a shape to a space."
  (when (shape-space shape)
    (error "This shape is already added to a space. Remove it first with space-remove-shape"))
  (cp:space-add-shape (base-c space) (base-c shape))
  (push shape (space-shapes space))
  (setf (shape-space shape) space))

(defmethod space-remove-shape ((space space) (shape shape))
  "Remove a shape from a space."
  (when (eq (shape-space shape) space)
    (cp:space-remove-shape (base-c space) (base-c shape))
    (setf (shape-space shape) nil)))

(defmethod space-add-joint ((space space) (joint joint))
  "Add a joint to a space."
  (when (joint-space joint)
    (error "This joint is already added to a space. Remove it first with space-remove-joint"))
  (cp:space-add-constraint (base-c space) (base-c joint))
  (push joint (space-joints space))
  (setf (joint-space joint) space))

(defmethod space-remove-joint ((space space) (joint joint))
  "Remove a joint from a space."
  (when (eq (joint-space joint) space)
    (cp:space-remove-constraint (base-c space) (base-c joint))
    (setf (joint-space joint) nil)))

(defmethod space-step ((space space) &key (dt +dt+))
  "Runs the physics simulation using a fixed step based on the +dt+ value."
  (let ((timer (space-timer space)))
    (let* ((new-time (get-internal-real-time))
           (frame-time (- new-time (getf timer :cur-time)))
           (dt-adjusted (* dt internal-time-units-per-second)))
      (setf (getf timer :cur-time) new-time)
      (incf (getf timer :accum) frame-time)
  
      (loop while (>= (getf timer :accum) dt-adjusted) do
        (cp:space-step (base-c space) +dt+)
        (decf (getf timer :accum) dt-adjusted)))))

(defmethod sync-space-bodies ((space space))
  "For each body within a space, sync its values (position, angle, etc) with its
  chipmunk counterpart."
  (dolist (body (space-bodies space))
    (sync-body body)))

