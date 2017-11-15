#|
==================================================================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: c-snp-with-vars.lisp
Author: Vladimir Kulyukin
Description: A semantic network processor for token
             sequences with variables.
2000, 2002

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
===================================================================
|#

(in-package "SNP")

(eval-when (load eval compile)
  (unless (find-package "SNP")
    (make-package "SNP")))

(in-package  "SNP")
(use-package "USER")

(defclass c-snp-with-vars (c-snp) ())

(defmethod define-token-seq ((this-snp c-snp-with-vars) 
                             (frame t) 
                             (tok-seq list)
                             &key (test #'eql))
  "Overloaded define-token-seq to process variables and tests."
  (with-slots (m-frame-manager) this-snp
    (let ((processed-tokseq (add-tokseq m-frame-manager frame tok-seq)))
      (unless (and (funcall test frame (first tok-seq))
                   (null (rest toke-seq)))
        (let ((exp (make-static-expectation :frame frame
                                            :tok-seq processed-tokseq)))
          (add-expectation this-snp exp))))))

(defmethod expectations-on-token ((this-snp c-snp-with-vars) (tok t))
  "Overloaded expectations-on-token to process variables and tests."
  "Get all expectations waiting for the token tok."
  `(,@(find-static-expectations this-snp tok)
       ,@(find-dynamic-expectations this-snp tok)))

(defmethod find-static-expectations ((this-snp c-snp-with-vars) (tok t))
  (with-slots (m-static-expectations) this-snp
    (let ((exps (get-static-expectations tok m-static-expectations)))
      (map-static-expectations
       #'(lambda (key exp-list)
           (when (user:variable-p key)
             (loop
               for e in exp-list
               when (var-pred-satisfied-p this-snp (m-frame e) key tok)
               do (pushnew e exps))))
       m-static-expectations)
      exps)))

(defmethod find-dynamic-expectations ((this-snp c-snp-with-vars) (tok t))
  (with-slots (m-dynamic-expectations) this-snp
    (let ((exps (filter-dynamic-expectations 
                 (get-dynamic-expectations tok
                                           m-dynamic-expectations))))
      (map-dynamic-expectations
       #'(lambda (key exp-list)
           (when (user:variable-p key)
             (loop
               for e in exp-list
               when (and (m-active-p e)
                         (var-pred-satisfied-p this-snp (m-frame e) key tok))
               do (pushnew e exps))))
       m-dynamic-expectations)
      exps)))

(defmethod var-pred-satisfied-p ((this-snp c-snp-with-vars) (frame t) (var symbol) (tok t))
  "Run the predicate associated with var."
  (with-slots (m-frame-manager) this-snp
    (let ((pred (get-var-pred m-frame-manager frame var)))
      (funcall pred tok))))
                      
(defun toggle-snp-with-vars (&key frames tokseqs)
  (setf *snp* (make-instance 'c-snp-with-vars 
               :m-frame-manager
               (toggle-fm-with-vars frames)))
  (install-tokseqs *snp* tokseqs)
  *snp*)

(defun re-toggle-snp-with-vars (&key frames tokseqs)
  (setf (m-frame-manager *snp*)
    (toggle-fm-with-vars frames))
  (install-tokseqs *snp* tokseqs)
  *snp*)

;;; end-of-file
