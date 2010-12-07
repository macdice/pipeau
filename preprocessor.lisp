(in-package :pipeau)

(defun replace-trigraphs (line)
  "Replace trigraphs with the characters they represent."
  ;; TODO
  line)

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
