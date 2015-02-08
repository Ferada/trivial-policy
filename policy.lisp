(in-package #:trivial-policy)

(defun policies ()
  "Return an ALIST of policy setting and the corresponding numeric value."
  #+sbcl sb-c::*policy*
  #-(or sbcl)
  (mapcar (lambda (name) (cons name (policy name))) (qualities)))

(defun qualities ()
  (or
   #+sbcl sb-c::*policy-qualities*
   '(speed safety debug space compilation-speed)))

(defun policy (name)
  "Return the numeric value for the policy setting NAME.  See POLICIES."
  (or
   #+abcl
   (case name
     (safety system:*safety*)
     (space system:*space*)
     (speed system:*speed*)
     (debug system:*debug*))
   #-(or abcl)
   (cdr (assoc name (policies) :test #'eq))
   (case name ((speed safety debug space compilation-speed) 1))
   (error "No policy setting named ~A." name)))

(defmacro policy-case (name &body cases)
  (let ((body (or (cdar (member-if (lambda (case &aux (car (car case)))
                                     (member (policy name) (if (atom car) (list car) car)))
                                   cases))
                  (let ((last (car (last cases))))
                    (and (eq (car last) T)
                         (cdr last))))))
    (and body `(progn ,@body))))

(defun %compare-policy (name value operator body)
  (and (funcall operator (policy name) value)
       `(progn ,@body)))

(defmacro policy<= (name value &body body)
  (%compare-policy name value #'<= body))

(defmacro policy>= (name value &body body)
  (%compare-policy name value #'>= body))
