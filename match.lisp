(in-package :pipeau)

;;; This is my cheap immitation of real pattern matching systems like
;;; match-s48, Erlang, Haskell etc.  A real pattern matching macro
;;; would expand to efficient inline decision code, but this isn't one
;;; of those systems, it just tests each pattern sequentially.  Ported
;;; from some old Emacs Lisp code I wrote years ago (wok-match.el).
;;;
;;; Usage:
;;;
;;; (match <object>
;;;   (<pattern>
;;;    <body> ...)
;;;   (<pattern>
;;;    <body> ...)
;;;   ...)
;;;
;;; Patterns are composed of the following objects:
;;;
;;; * the symbol _ matches and ignores any object
;;; * quoted symbols match equal symbols
;;; * strings match equal strings
;;; * numbers match equal numbers
;;; * lists match lists of the same size, if their elements match
;;; * improper lists allow the tail of a list to be matched 
;;; * unquoted symbols match any object and are bound to the value in the body
;;;
;;; TODO - support equality testing, when a name is used more than once?
;;; TODO - support regex patterns for matching/decomposing strings
;;; TODO - support arbitrary predicates (like the 'if' in Erlang and Scala)
;;; TODO - support symbols with name "_" from any package as wildcard?

(defun match-quoted-symbol-p (object)
  "Tests if an object is a quoted symbol."
  (and (consp object)
       (eq (car object) 'quote)
       (consp (cdr object))
       (symbolp (cadr object))))

(defun match-test (pattern candidate)
  "Tests if an object consisting of nested lists and atoms matches
   a pattern."
  (cond ((null pattern)
         (null candidate))
        ((match-quoted-symbol-p pattern)
         (eq (cadr pattern) candidate))
        ((consp pattern)
         (and (consp candidate)
              (match-test (car pattern) (car candidate))
              (match-test (cdr pattern) (cdr candidate))))
        ((symbolp pattern) t)
        ((stringp pattern)
         (and (stringp candidate)
              (string= pattern candidate)))
        ((numberp pattern)
         (and (numberp candidate)
              (= pattern candidate)))
        (t nil)))

(assert (match-test 42 42))
(assert (match-test "hello" "hello"))
(assert (match-test ''hello 'hello))
(assert (match-test 'hello 'goodbye)) 
(assert (match-test '(a b) '(a b)))
(assert (match-test '(_ _) '(a b)))
(assert (match-test '() '()))
(assert (match-test '(_ _ (_ _)) '(a b (c d))))
(assert (match-test '(_ _ _) '(a b (c d))))
(assert (match-test '(_ _ x) '(a b (c d))))
(assert (match-test '(_ . _) '(a b (c d))))
(assert (match-test '(_ _ . _) '(a b (c d))))
(assert (match-test '(_ _ _ . _) '(a b (c d))))
(assert (not (match-test 42 999)))
(assert (not (match-test "hello" "goodbye")))
(assert (not (match-test ''hello 'goodbye)))
(assert (not (match-test '(a b c) '(a b))))
(assert (not (match-test '(a) '(a b))))
(assert (not (match-test '(_ _ (_ _)) '(a b (c)))))
(assert (not (match-test '(_ _ _ _ . _) '(a b (c d)))))

(defun match-make-path (pattern name path)
  "Given a nested pattern and a name, returns a path expression that can be 
   used to access that element of the nested structure.  For example, the 
   pattern (a (b 42 c)) produces the following results for each name:
     a -> (car object) 
     b -> (car (car (cdr object))) 
     c -> (car (car (cdr (cdr (cdr (car (cdr object)))))))."
  (cond ((null pattern) nil)
        ((eq pattern name) path)
        ((match-quoted-symbol-p pattern) nil)
        ((consp pattern)
         (let ((first (match-make-path (car pattern) 
                                           name 
                                           (list 'car path))))
           (if first
               first
             (match-make-path (cdr pattern) 
                                  name
                                  (list 'cdr path)))))
        (t nil)))

(assert (null (match-make-path '(a (b 42 c)) 'z 'object)))
(assert (equal (match-make-path '(a (b 42 c)) 'a 'object) '(car object)))
(assert (equal (match-make-path '(a (b 42 c)) 'b 'object)
               '(car (car (cdr object)))))
(assert (equal (match-make-path '(a b (c d)) 'd 'object)
               '(car (cdr (car (cdr (cdr object)))))))
(assert (equal (match-make-path '(a . b) 'b 'object)
               '(cdr object)))
(assert (equal (match-make-path '(a b . c) 'c 'object)
               '(cdr (cdr object))))

(defun match-find-names (pattern)
  "Returns a list of capturing names (unquoted symbols excluding _) found in 
   a nested pattern structure."
  (cond ((null pattern) 
         nil)
        ((match-quoted-symbol-p pattern)
         nil)
        ((consp pattern) 
         (append (match-find-names (car pattern))
                 (match-find-names (cdr pattern))))
        ((symbolp pattern)
         (if (eq pattern '_)
             nil
           (list pattern)))
        (t nil)))

(assert (equal (match-find-names '(a b (c d (e f)))) '(a b c d e f)))
(assert (equal (match-find-names '(a _ "hello" 'x 42 b)) '(a b)))
(assert (equal (match-find-names '(a b (c d (e . f)))) '(a b c d e f)))
(assert (equal (match-find-names '(a b (c d . (e . f)))) '(a b c d e f)))

(defmacro match* (object &rest clauses)
  "The guts of the recursive pattern matcher.  Use via match if you don't
   want <object> to be evaluated repeatedly."
  (cond ((null clauses) 'nil)
        ((consp clauses)
         (let ((head (car clauses)))
           (if (consp head)
               (let ((pattern (car head))
                     (body (cdr head)))
                 `(if (match-test (quote ,pattern) ,object)
                      (let ,(mapcar 
                             (lambda (name)
                               (list name (match-make-path pattern 
                                                               name 
                                                               object)))
                             (match-find-names pattern))
                        ,@body)
                    (match* ,object ,@(cdr clauses))))
             (error "bad match clause, expected (pattern body ...)"))))
        (t (error "match expected at least one clause"))))

(defmacro match (object &rest clauses)
  "Evaluates the body associated with the first pattern matching <object>.
   Clauses are of the form (<pattern> <body> ...).  Object is evaluated once."
  (let ((name (gensym)))
    `(let ((,name ,object))
       (match* ,name ,@clauses))))

(assert (equal (match '(hello foo)
                 (('hello name)
                  (format nil "hi there ~a" name))
                 (_ "non capisco"))
               "hi there FOO"))
(assert (null (match '(zarp "scott" "tiger")
                (('logoff)
                 (format nil "goodbye"))
                (('logon username password)
                 (format nil "hi there ~a" username)))))
(assert (equal (match '(a b c)
                 ((x . xs) (format nil "head = ~a tail = ~a" x xs)))
               "head = A tail = (B C)"))
