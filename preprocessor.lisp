(in-package :pipeau)

(defconstant trigraphs
  '((#\( . "[")
    (#\) . "]")
    (#\< . "{")
    (#\> . "}")
    (#\= . "#")
    (#\/ . "\\")
    (#\' . "^")
    (#\! . "|")
    (#\- . "~")
    (#\\ . "??")) ; This last one is a GNU extension, though it should only be valid inside a string.
  "List of trigraph mappings.")

(defun maybe-consume-trigraph (line pos)
  "Look for a question mark and a trigraph character. If found, and valid, return the substition, else ??x."
  (if (char= #\? (schar line pos))
      (let ((tri (assoc (schar line (1+ pos)) trigraphs)))
        (if tri
            (cdr tri) ; Found one.
            (subseq line (1- pos) (+ pos 2))))))

(defun replace-trigraphs (line)
  "Return line with all trigraphs converted to their standard character replacements."
  (let* ((max-pos (length line))
         (tri-break (- max-pos 2))) ; Where to quit looking for trigraphs in a line.
    (labels ((replace-impl (out pos)
               (if (< pos max-pos)
                   (let ((ch (schar line pos)))
                     (if (and (< pos tri-break) (char= #\? ch))
                         (progn (write-string (maybe-consume-trigraph line (1+ pos)) out)
                                (replace-impl out (+ pos 3)))
                         (progn (write-char ch out)
                                (replace-impl out (1+ pos))))))))
      (with-output-to-string (out) (replace-impl out 0)))))

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
