(in-package :pipeau)

(defstruct process
  "A record type for processes, which link a mailbox and a thread."
  (:name)
  (:thread)
  (:mailbox))

(defparameter *self* (make-process :name "main" 
                                   :mailbox (make-mailbox))
  "A special variable holding the current process.")

(defun spawn (function &optional name)
  "Spawn a new process."
  (let* ((mailbox (make-mailbox :name name))
         (process (make-process :mailbox mailbox :name name))
         (thread (bordeaux-threads:make-thread 
                  (lambda ()
                    (setf *self* process)
                    (funcall function))
                  :name name)))
    (setf (process-thread process) thread) ;; a bit circular!
    process))

(defun join (process)
  "Wait for a process to terminate."
  (bordeaux-threads:join-thread (process-thread process)))

(defun self ()
  "Return the current process's mailbox."
  *self*)

(defun ? (&optional timeout default)
  "Receive a message from one's own mailbox."
  (mailbox-receive-if (process-mailbox *self*) (lambda (x) t) timeout default))

;;(defun ?* ()
;;  "Receive all messages currently waiting in one's mailbox without blocking."
;;  (sb-concurrency:receive-pending-messages (process-mailbox *self*)))

(defun ! (process message)
  "Send a message to a process."
  (mailbox-send message (process-mailbox process)))
