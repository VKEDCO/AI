#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: USER -*-

File: snp-loader.lisp
Author: Vladimir Kulyukin
Description: semantic network processor loader
2000, 2002

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#

(in-package "USER")

(defparameter parm-snp-load-dir 
  (directory-namestring *load-truename*))

(defparameter parm-snp-files
  '("utils" "deftable" "c-frame-manager" 
    "c-snp" "c-snp-with-input"
    "c-snp-with-vars" 
    "c-fm-with-vars"))

(defun snp-load-everything (&key (type :lisp))
  (mapc #'(lambda (f)
            (load (make-pathname 
                    :directory parm-snp-load-dir 
                    :name f
                    :type (ecase type
                            (:lisp "lisp")
                            (:cl "cl")
                            (:fasl "fasl")))
                  :print t))
    parm-snp-files))

(defun snp-compile-everything (&key (type :lisp))
  (mapc #'(lambda (f)
            (compile-file (make-pathname
                            :directory parm-snp-load-dir 
                            :name f
                            :type (ecase type
                                    (:lisp "lisp")
                                    (:cl "cl")
                                    (:fasl "fasl")))))
    parm-snp-files))

;;; end-of-file
