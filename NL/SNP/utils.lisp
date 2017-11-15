#|
=================================================================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: USER -*-

File: utils.lisp
Author: Vladimir Kulyukin
Description: Utility methods and functions.

Acknowledgments:

1. The ideas behind to-symbols and to-list are borrowed
from William Fitzgerald's implementation of DMAP. Neither
to-symbols or to-list are used anywhere in the Semantic
Network Processor code. However, they may be useful
when the Semantic Network Processor must be connected
to the other systems.

2. mappend and variable-p are well-known parts of the Lisp lore.
Many books on AI that use Lisp give implementations
of mappend and variable-p.
=================================================================
|#

(in-package "USER")

(export '(to-symbols to-list mappend variable-p))

(defmethod to-symbols ((x string) &key (package *package*))
  "Converts a string to symbols interned in package."
   (to-list x
    :post-process #'(lambda (str)
                      (intern (nstring-upcase str) package))
    :test #'(lambda (ch) (or (alphanumericp ch)
                             (char= ch #\-)))))

(defmethod to-symbols ((x null) &key (package *package*))
  (declare (ignore package)) nil)

(defmethod to-list ((self string) &key
                    (start 0)
                    (break-chars '(#\space))
                    (test #'(lambda (ch) 
                              (not (find ch break-chars :test #'char=))))
                    (post-process #'identity))
   "Converts a string into a list."
   (labels ((next-break-position (self pos)
             (position-if-not test self :start pos))

            (next-char-position (self pos)
             (unless (null pos)
                (position-if test self :start pos)))

            (to-list-local (position)
             (let* ((break-pos (next-break-position self position))
                    (char-pos  (next-char-position self break-pos)))


                (if break-pos
                   (if char-pos
                      (cons (funcall post-process 
                                     (subseq self position break-pos))
                            (to-list-local char-pos))
                      (list (funcall post-process 
                              (subseq self position break-pos))))
                   (list (funcall post-process (subseq self position)))))))
     (let ((char-pos (next-char-position self start)))
        (if char-pos 
           (to-list-local char-pos) 
          nil))))

(defun mappend (fn list)
  "Call fn on each element of the list and append the results into a new list."
  (apply #'append (mapcar fn list)))

(defun variable-p (var)
  "Is var a variable symbol?"
  (and (symbolp var)
       (char= (char (symbol-name var) 0) #\?)))


;;; end-of-file
