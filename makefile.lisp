;; To build a standalone executable:
;;
;; sbcl --load makefile.lisp

(require :sb-concurrency)
(load "package.lisp")
(in-package :pipeau)

(defun main ()
  (format t "Pipeau~%")
  (sb-ext:quit :unix-status 0))

(sb-ext:save-lisp-and-die "pipeau" :toplevel #'main :executable t)
