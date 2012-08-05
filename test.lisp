(ql:quickload :chipmunk-wrapper)

(defpackage :chipmunk-wrapper-test
  (:use :cl))
(in-package :chipmunk-wrapper-test)

(defun simple ()
  (let ((space (cpw:make-space :gravity-y -9.8)))
    (let ((ground (cpw:make-shape :segment
                                  (cpw:space-static-body space)
                                  (lambda (body)
                                    (cpw:shape-segment body -100 0 100 0 3)))))
      (setf (cp-a:shape-u (cpw:base-c ground)) 1.0d0)
      (cpw:space-add-shape space ground)
      (let* ((box-body (cpw:make-body (lambda () (cp:body-new 1d0 (cp:moment-for-circle 1d0 0d0 3d0 0d0 0d0)))))
             (box-shape (cpw:make-shape :poly box-body (lambda (body) (cpw:shape-poly body #((-5 -5) (-5 5) (5 5) (5 -5)) 0 0)))))
        (cp:body-set-pos (cpw:base-c box-body) 0d0 100d0)
        (cp:body-set-angle (cpw:base-c box-body) (* 30 (/ PI 180)))
        (setf (cp-a:shape-u (cpw:base-c box-shape)) 0.7d0)
        (cpw:space-add-body space box-body)
        (cpw:space-add-shape space box-shape)
        (loop for time from 0 to 2 by cpw:+dt+ do
          (cpw:sync-space-bodies space)
          (format t "Body: (~a, ~a) rot: ~a~%" (cpw:body-x box-body) (cpw:body-y box-body) (cpw:body-angle box-body))
          (sleep 0.1)
          (cpw:space-step space))
        (cpw:destroy box-shape)
        (cpw:destroy box-body))
      (cpw:destroy ground)
      (cpw:destroy space))))

(defun poly (verts)
  (cffi:with-foreign-object (vects 'clipmunk:vect (length verts))
    (loop for i from 0
          for p across verts do
      (let ((x (coerce (car p) cpw:+physics-precision+))
            (y (coerce (cadr p) cpw:+physics-precision+)))
        (setf (cp-a:vect-x (cffi:mem-aref vects 'clipmunk:vect i)) x
              (cp-a:vect-y (cffi:mem-aref vects 'clipmunk:vect i)) y)))
    (cp:poly-shape-new (cp:body-new 1d0 1d0) (length verts) vects 0d0 0d0)))

