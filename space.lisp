(in-package :chipmunk-wrapper)

(defconstant +dt+ (coerce (/ 1 60) 'double-float))

(defclass space (base)
  ((c-type :initform :space)
   (bodies :accessor space-bodies :initform nil)
   (shapes :accessor space-shapes :initform nil)
   (joints :accessor space-joints :initform nil)))

(defun make-space ()
  "Stupid wrapper around space creation."
  (let ((c-obj (cp:space-new)))
    (make-instance 'space :c c-obj)))

(defun space-add-body (space body)
  "Add a body to a space."
  (cp:space-add-body (base-c space) (base-c body))
  (push body (space-bodies space)))

(defun space-add-shape (space shape)
  "Add a shape to a space."
  (cp:space-add-shape (base-c space) (base-c shape))
  (push shape (space-shapes space)))

(defun space-add-joint (space joint)
  "Add a joint to a space."
  (cp:space-add-constraint (base-c space) (base-c joint))
  (push joint (space-joints space)))

(defparameter *cur-time* nil)
(defparameter *accumulator* 0)

(defun space-step (space)
  "Runs the physics simulation using a fixed step based on the +dt+ value."
  (unless *cur-time*
    (setf *cur-time* (get-internal-real-time)))
  (let* ((new-time (get-internal-real-time))
         (frame-time (/ (- new-time *cur-time*) internal-time-units-per-second)))
    (setf *cur-time* new-time)
    (incf *accumulator* frame-time)

    (loop while (>= *accumulator* +dt+) do
      (cp:space-step (base-c space) +dt+)
      (decf *accumulator* +dt+))))

