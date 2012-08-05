(in-package :chipmunk-wrapper)

(defconstant +shape-types+ '(:circle :segment :poly))

(defclass shape (base)
  ((c-type :initform :shape)
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
  (assert (subtypep (type-of body) 'body))
  (cp:circle-shape-new (base-c body)
                       (coerce radius +physics-precision+)
                       (coerce offset-x +physics-precision+)
                       (coerce offset-y +physics-precision+)))

(defun shape-segment (body p1x p1y p2x p2y radius)
  "Wrapper around the creation of a segment shape."
  (assert (subtypep (type-of body) 'body))
  (cp:segment-shape-new (base-c body)
                        (coerce p1x +physics-precision+)
                        (coerce p1y +physics-precision+)
                        (coerce p2x +physics-precision+)
                        (coerce p2y +physics-precision+)
                        (coerce radius +physics-precision+)))

(defun shape-polygon (body verts offset-x offset-y)
  "Wrapper around creation of a polygon shape. Currently unimplemented."
  (declare (ignore verts offset-x offset-y))
  (assert (subtypep (type-of body) 'body))
  (error "Polygon shapes are not currently supported."))
