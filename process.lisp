(in-package :pipeau)

(defstruct process
  "A record type for processes."
  (:name)
  (:thread)
  (:mailbox)
  (:links)
  (:trap-exits))

(defparameter *self* (make-process :name "main" 
                                   :mailbox (make-mailbox))
  "A special variable holding the current process.")

(defun spawn (function &optional name)
  "Spawn a new process."
  (let* ((mailbox (make-mailbox :name name))
         (process (make-process :mailbox mailbox :name name))
         (thread (bordeaux-threads:make-thread 
                  (lambda ()
                    (let ((*self* process)) ;; thread-local rebind of special
                      (unwind-protect
                           (funcall function)
                        (loop for link in (process-links (self))
                             do (! link `(EXIT ,(self)))))))
                  :name name)))
    (setf (process-thread process) thread) ;; a bit circular!
    process))

(defun join (process)
  "Wait for a process to terminate."
  (bordeaux-threads:join-thread (process-thread process)))

(defun self ()
  "Return the current process."
  *self*)

(defun trap-exits (&optional (active t))
  "Register to receive exit messages from linked processes explicitly,
rather than getting an error."
  (setf (process-trap-exits active)))

(defun ? (&optional timeout default)
  "Receive a message from one's own mailbox."
  (mailbox-receive-if (process-mailbox *self*) (lambda (x) t) timeout default))

(defun ! (process message)
  "Send a message to a process."
  (mailbox-send message (process-mailbox process)))
