(in-package :pipeau)

(defconstant trichars
  '((#\( . #\[)
    (#\) . #\])
    (#\< . #\{)
    (#\> . #\})
    (#\= . #\#)
    (#\/ . #\\)
    (#\' . #\^)
    (#\! . #\|)
    (#\- . #\~)
    (#\\ . #\?)) ; This last one is a GNU extension, though it should only be valid inside a string.
  "List of trigraph mappings.")

(defun replace-trigraphs (line)
  "Return line with all trigraphs converted to their standard character replacements."
  (labels ((check-for-trigraph (p end)
             (if (< (+ p 2) end) ; Don't go off the end.
                 (if (and (char= #\? (schar line p)) (char= #\? (schar line (1+ p))))
                     (assoc (schar line (+ p 2)) trichars))))
           (iter (r w end)
             (if (< r end)
                 (let ((tri (check-for-trigraph r end)))
                   (cond 
                     ((null tri) ; No trigraph found.
                      (setf (schar line w) (schar line r))
                      (iter (1+ r) (1+ w) end))
                     ((char= #\? (cdr tri)) ; Special GNU extension: consume the backslash.
                      (setf (schar line w) (schar line r))
                      (setf (schar line (1+ w) ) (schar line (1+ r)))
                      (iter (+ r 3) (+ w 2) end))
                     (t (setf (schar line w) (cdr tri)) ; Replace ??x sequence with character.
                        (iter (+ r 3) (1+ w) end))))
                 (subseq line 0 w))))
    (iter 0 0 (length line))))

(defun preprocess (path target)
  "Read in a file line by line, perform simple preprocessing, and send
the resulting lines to actor TARGET, followed by the symbol :EOF."
  ;; TODO: should we tokenize to lists of tokens, or to strings with
  ;; white spaces in them, like a normal tokenizer?
  (with-open-file (stream path)
    (let ((multi-line nil)
          (line-resync-needed nil))
          
      (loop 
         for input-line-number from 1
         for line = (read-line stream nil)
         while line do
           (when multi-line
             (setf line (concatenate 'string multi-line line))
             (setf multi-line nil))
           (cond ((char= #\\ (char line (1- (length line))))
                  (setf multi-line (subseq line 0 (1- (length line))))
                  (setf line-resync-needed t))
                 (t
                  (! target line)
                  (when line-resync-needed
                    (! target (format nil "#line ~a" input-line-number))
                    (setf line-resync-needed nil)))))))
  (! target :eof))
