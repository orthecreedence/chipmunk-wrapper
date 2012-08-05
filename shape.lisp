(in-package :chipmunk-wrapper)

(defconstant +shape-types+ '(:circle :segment :poly))

(defclass shape (base)
  ((c-type :initform :shape)
   (space :accessor shape-space :initform nil)
   (type :accessor shape-type :initarg :type :initform nil)
   (body :accessor shape-body :initarg :body :initform nil)))

(defmethod make-shape ((type keyword) (body body) (create-fn function))
  "Create a shape. The actual shape creation is completely dependent on the type
  of shape being made, so the second argument is a callback which returns a CFFI
  pointer to a new shape."
  (let ((c-obj (funcall create-fn body)))
    (assert (cffi:pointerp c-obj))
    (make-instance 'shape :c c-obj :type type :body body)))

(defmethod shape-circle ((body body) (radius number) (offset-x number) (offset-y number))
  "Wrapper around the creation of a circle shape."
  (cp:circle-shape-new (base-c body)
                       (coerce radius +physics-precision+)
                       (coerce offset-x +physics-precision+)
                       (coerce offset-y +physics-precision+)))

(defmethod shape-segment ((body body) (p1x number) (p1y number) (p2x number) (p2y number) (radius number))
  "Wrapper around the creation of a segment shape."
  (cp:segment-shape-new (base-c body)
                        (coerce p1x +physics-precision+)
                        (coerce p1y +physics-precision+)
                        (coerce p2x +physics-precision+)
                        (coerce p2y +physics-precision+)
                        (coerce radius +physics-precision+)))

(defmethod shape-poly ((body body) (verts vector) (offset-x number) (offset-y number))
  "Wrapper around creation of a polygon shape. Currently unimplemented."
  ;; the chipmunk poly shape copies the verts into its own memory, so we can
  ;; safely release the memory after creating the poly shape.
  (cffi:with-foreign-object (vects 'clipmunk:vect (length verts))
    (loop for i from 0
          for p across verts do
      (let ((x (coerce (car p) cpw:+physics-precision+))
            (y (coerce (cadr p) cpw:+physics-precision+)))
        (setf (cp-a:vect-x (cffi:mem-aref vects 'clipmunk:vect i)) x
              (cp-a:vect-y (cffi:mem-aref vects 'clipmunk:vect i)) y)))
    (cp:poly-shape-new (base-c body) (length verts) vects (coerce offset-x +physics-precision+) (coerce offset-y +physics-precision+))))

