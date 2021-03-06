(defpackage :chipmunk-wrapper
  (:use :cl :alexandria)
  (:nicknames :cpw)
  (:shadow cl:space)
  (:export #:+physics-precision+
           #:+dt+
           
           #:base
           #:base-c
           #:destroy

           #:body
           #:body-space
           #:body-shapes
           #:body-x
           #:body-y
           #:body-angle
           #:body-data
           #:make-body
           #:sync-body
           #:body-sleeping-p
           #:body-static-p
           #:find-body-from-pointer

           #:shape
           #:shape-space
           #:shape-type
           #:shape-body
           #:make-shape
           #:shape-circle
           #:shape-segment
           #:shape-poly
           #:moment-for-poly

           #:joint
           #:joint-space
           #:joint-body1
           #:joint-body2
           #:make-joint
           #:detach-joint

           #:space
           #:space-static-body
           #:space-bodies
           #:space-shapes
           #:space-joints
           #:make-space
           #:space-add-body
           #:space-remove-body
           #:space-add-shape
           #:space-remove-shape
           #:space-add-joint
           #:space-remove-joint
           #:space-step
           #:sync-space-bodies
           
           #:arbiter
           #:arbiter-c
           #:arbiter-elasticity
           #:arbiter-friction
           #:arbiter-surface-velocity
           #:arbiter-contact-count
           #:arbiter-normals
           #:arbiter-points
           #:arbiter-first-contact-p
           #:arbiter-ignore-collision
           #:make-arbiter
           #:get-arbiter-normal
           #:get-arbiter-point
           #:sync-arbiter-to-c))
