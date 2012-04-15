(in-package :pipeau)

;; A blocking queue, with a pattern-matching interface.  Unlike
;; typical blocking queues, it is possible to leave objects in the
;; queue.  Currently the implementation is inefficient, churning cons
;; cells and chasing pointers.

(defstruct mailbox
  "A record type for mailboxes."
  (:name)
  (:lock (bordeaux-threads:make-lock))
  (:condition-variable (bordeaux-threads:make-condition-variable))
  (:front nil)
  (:back nil))

(defun mailbox-send (object mailbox)
  "Send OBJECT to MAILBOX."
  (bordeaux-threads:with-lock-held ((mailbox-lock mailbox))
    (let ((cell (cons object nil)))
      (if (mailbox-back mailbox)
          (setf (cdr (mailbox-back mailbox)) cell
                (mailbox-back mailbox) cell)
          (setf (mailbox-back mailbox) cell
                (mailbox-front mailbox) cell)))
    (bordeaux-threads:condition-notify (mailbox-condition-variable mailbox))))

(defun mailbox-try-receive-if (mailbox predicate default)
  "Remove and return the first object from MAILBOX that satisfies
PREDICATE, or return the value in DEFAULT if none matches.  Private,
assumes that MAILBOX is locked."
  ;; TODO I'm sure there must be a way to manage last-cell using loop-fu,
  ;; and a way to get rid of the COND form
  (let ((last-cell nil))
    (loop
       for cell on (mailbox-front mailbox)
       for value = (car cell)
       do (cond ((funcall predicate value)
                 ;; remove cell from the chain
                 (if last-cell
                     (setf (cdr last-cell) (cdr cell))
                     (setf (mailbox-front mailbox) (cdr cell)))
                 ;; if it was the one that the back pointer was
                 ;; pointing to then update the back pointer
                 (when (eq cell (mailbox-back mailbox))
                   (setf (mailbox-back mailbox) last-cell))
                 (return value))
                (t (setf last-cell cell)))         
       finally (return default))))

(defun mailbox-receive-if (mailbox predicate &optional timeout timeout-value)
  "Return the first object from MAILBOX that satisfies PREDICATE, or
wait up as long as TIMEOUT until one is available.  If the timeout is
reached, then TIMEOUT-VALUE is returned.  This operations skips
non-matching objects, leaving them in the mailbox."

  ;; TODO bordeaux-threads doesn't seem to support waiting for
  ;; conditional variables with a timeout so for now timeout has no
  ;; effect except for the special value of 0 which means 'do not
  ;; wait'.

  ;; TODO each time we have to wait, we reevaluate all the queued up
  ;; things we've already evaluated, which is stupid; we should
  ;; somehow keep a cursor pointing to the bit we've reached and start
  ;; from there next time round the loop

  ;; TODO the point of having a receive-if that leaves objects in the
  ;; queue (ie allows things to be received out of sequence) is to
  ;; allow for the 'tags' of Termite Scheme, a way to do RPC, and also
  ;; to handle higher priority messages sooner than queued up lower
  ;; priority ones; but that functionality isn't being provided in any
  ;; useful way just yet!

  (bordeaux-threads:with-lock-held ((mailbox-lock mailbox))
    (loop do
         (let ((value (mailbox-try-receive-if mailbox predicate '%not-found)))
           (cond ((not (eq value '%not-found))
                  (return value))
                 ((and timeout (zerop timeout))
                  (return timeout-value))
                 (t
                  (bordeaux-threads:condition-wait (mailbox-condition-variable mailbox)
                                                   (mailbox-lock mailbox))))))))       
