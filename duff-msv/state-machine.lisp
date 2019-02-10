(in-package :net.earthlink.dkixk.duff-msv)

;;; The following is pretty much all there is to it.

(defgeneric handle-event (state-machine event))

(defgeneric transition (state event))

(defgeneric final-state-p (object))

(defclass state-machine ()
  ((state :initarg :start-state :accessor state)))

(defmethod handle-event ((state-machine state-machine)
                         event)
  (with-accessors ((state state)) state-machine
    (setf state (transition state event))))

(defmethod final-state-p ((object state-machine))
  (final-state-p (state object)))
