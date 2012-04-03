(in-package :pipeau)

(defstruct process
  "A record type for processes."
  (:name)
  (:thread)
  (:mailbox)
  (:links)
  (:trap-exit)
  (:lock (bordeaux-threads:make-lock)))

(defparameter *self* (make-process :name "main" 
                                   :mailbox (make-mailbox))
  "A special variable holding the current process.")

(defun self ()
  "Return the current process."
  *self*)

(defun spawn (function &optional name)
  "Spawn a new process."
  (let* ((mailbox (make-mailbox :name name))
         (process (make-process :mailbox mailbox :name name))
         (thread (bordeaux-threads:make-thread 
                  (lambda ()
                    (let ((*self* process)) ;; thread-local rebind of special
                      (unwind-protect
                           (funcall function)
                        ;; TODO lock
                        (loop for link in (process-links (self))
                             do (mailbox-send `(EXIT ,(self)) link)))))
                  :name name)))
    ;; TODO does the following break rules about visibility?
    ;; who is supposed to see this value?  noone else should
    ;; update it and it will only ever be used to join
    (setf (process-thread process) thread)
    process))

(defun link (process)
  "Create a bidirectional link between the calling process and the
named process."
  (bordeaux-threads:with-lock-held ((process-lock (self)))
    (pushnew process (process-links (self))))
  (bordeaux-threads:with-lock-held ((process-lock process))
    (pushnew (self) (process-links process))))

(defun spawn-link (function &optional name)
  "Spawn a new process linked to the calling process."
  (let ((process (spawn function name)))
    (link process)
    process))

(defun join (process)
  "Wait for a process to terminate."
  (bordeaux-threads:join-thread (process-thread process)))

(defun trap-exit (&optional (active t))
  "Register to receive exit messages from linked processes explicitly,
rather than getting an error."
  (setf (process-trap-exit (self)) active))

(defun ? (&optional timeout default)
  "Receive a message from one's own mailbox."
  (mailbox-receive-if 
   (process-mailbox *self*)
   (lambda (x) (declare (ignorable x)) t) 
   timeout 
   default))

(defun ! (process message)
  "Send a message to a process."
  (mailbox-send message (process-mailbox process)))
