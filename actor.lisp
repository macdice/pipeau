;; A very primitive knock-off of Termite Scheme's operators, built on
;; top of SBCL's lock-free mailbox.  I'm using the word 'actor' rather
;; than 'process'.
;;
;; TODO:
;; - pattern matching 'recv'
;; - tags
;; - exceptions etc ...

(in-package :pipeau)
(require :sb-concurrency)

(defstruct actor
  "An actor representing and mailbox and a thread."
  (:name)
  (:thread)
  (:mailbox))

(defparameter *self* (make-actor :name "main" 
                                 :mailbox (sb-concurrency:make-mailbox))
  "A special variable holding the current actor.")

(defun spawn (function &optional name)
  "Spawn a new actor."
  (let* ((mailbox (sb-concurrency:make-mailbox))
         (actor (make-actor :mailbox mailbox :name name))
         (thread (sb-thread:make-thread 
                  (lambda ()
                    (setf *self* actor)
                    (funcall function)))))
    (setf (actor-thread actor) thread) ;; a bit circular!
    actor))

(defun join (actor)
  "Wait for an actor to terminate."
  (sb-thread:join-thread (actor-thread actor)))

(defun self ()
  "Return the current actor's mailbox."
  *self*)

(defun ? (&optional timeout default)
  "Receive a message from one's own mailbox."
  (sb-concurrency:receive-message (actor-mailbox *self*)))

(defun ?* ()
  "Receive all messages currently waiting in one's mailbox without blocking."
  (sb-concurrency:receive-pending-messages (actor-mailbox *self*)))

(defun ! (actor message)
  "Send a message to an actor."
  (sb-concurrency:send-message (actor-mailbox actor) message))

