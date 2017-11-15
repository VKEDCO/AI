#|
==============================================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: c-fm-with-vars.lisp
Author: Vladimir Kulyukin
Description: CLOS Frame System for token sequences with
             variables.
1994, 1997-98, 2000

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
==============================================
|#

(in-package "SNP")

(eval-when (load eval compile)
  (unless (find-package "SNP")
    (make-package "SNP")))

(in-package  "SNP")
(use-package "USER")

(deftable frames-var-preds)

(defclass c-fm-with-vars (c-frame-manager)
  ((m-frames-to-var-preds :initform *frames-var-preds-table*
                          :initarg  :m-frames-to-var-preds
                          :accessor m-frames-to-var-preds)))

(defmethod add-var-pred-pair :around ((this-fm c-fm-with-vars)
                                      (frame t)
                                      (var-pred-pair cons)
                                      &key (test #'eql))
  (if (get-var-pred this-fm frame (first var-pred-pair) :test test)
      (error "Frame ~A alread has the variable ~A~%" 
        (first var-pred-pair) (rest var-pred-pair))
    (call-next-method)))

(defmethod add-var-pred-pair ((this-fm c-fm-with-vars)
                              (frame t)
                              (var-pred-pair cons)
                              &key (test #'eql))
  (with-slots (m-frames-to-var-preds) this-fm
    (push var-pred-pair
          (get-frames-var-preds frame
                                m-frames-to-var-preds))))

(defmethod add-var-pred-pair ((this-fm c-fm-with-vars)
                              (frame t)
                              (var-pred-pair null)
                              &key (test #'eql))
  nil)

(defmethod add-var-pred-pairs ((this-fm c-fm-with-vars)
                               (frame t)
                               (var-pred-pair-list cons)
                               &key (test #'eql))
  (loop 
    for vp-pair in var-pred-pair-list
    do (add-var-pred-pair this-fm frame vp-pair)))

(defmethod add-var-pred-pairs ((this-fm c-fm-with-vars)
                               (frame t)
                               (var-pred-pair-list null)
                               &key (test #'eql))
  nil)

(defmethod add-var-pred ((this-fm c-fm-with-vars)
                         (frame t) (var t)
                         (pred t) &key (test #'eql))
  (with-slots (m-frames-to-var-preds) this-fm
    (let ((var-pred-list
           (get-frames-var-preds frame m-frames-to-var-preds)))
      (if (null var-pred-list)
          (push (cons var pred)
                (get-frames-var-preds frame m-frames-to-var-preds))
        (if (find frame var-pred-list :key #'first :test test)
            (error "Frame ~A already has the variable ~A~%" frame var)
          (push (cons var pred)
                (get-frames-var-preds frame m-frames-to-var-preds)))))))

(defmethod get-var-pred ((this-fm c-fm-with-vars)
                         (frame t) (var t) &key (test #'eql))
  (with-slots (m-frames-to-var-preds) this-fm
    (let ((var-pred-list
           (get-frames-var-preds frame m-frames-to-var-preds)))
      (if var-pred-list
          (rest (assoc var var-pred-list :test test))
        nil))))

(defmethod add-tokseq ((fm c-fm-with-vars) (frame t) (tokseq cons)
                       &key (test #'equal))
  "Add a token sequence to a frame."
  (multiple-value-bind (processed-tokseq var-pred-list)
      (pre-process-tokseq fm tokseq)
    (pushnew processed-tokseq 
             (get-frame-tokseqs frame (m-frame-to-tokseqs fm))
             :test test)
    (add-var-pred-pairs fm frame var-pred-list)
    processed-tokseq))

(defmethod pre-process-tokseq ((fm c-fm-with-vars) (tokseq cons))
  "Extract variables and tests associated with them."
  (loop
    with processed-tokseq = nil with var-pred-pairs = nil
    for x in tokseq
    when (var-pred-pair-p x)
    do (push x var-pred-pairs) (push (first x) processed-tokseq)
    else do (push x processed-tokseq)
    finally
    (setf processed-tokseq (nreverse processed-tokseq))
    (return (values processed-tokseq var-pred-pairs))))

(defmethod clear-frame-memory :after ((fm c-fm-with-vars))
  "Clears all frame info from the frame manager."
  (clear-frames-var-preds (m-frames-to-var-preds fm)))

(defun var-pred-pair-p (x)
  "Is x of the form (<var> . <test>)?"
  (if (and (consp x)
           (user:variable-p (first x))
           (fboundp (rest x)))
      t
    nil))

(defun init-fm-with-vars (&key frames)
  (setf *fm* (make-instance 'c-fm-with-vars))
  (load frames)
  *fm*)

(defmethod toggle-fm-with-vars ((frames pathname))
  (init-fm-with-vars :frames frames))

(defmethod toggle-fm-with-vars ((frames string))
  (init-fm-with-vars :frames frames))

(defmethod toggle-fm-with-vars :before ((frames cons))
  (setf *fm* (make-instance 'c-fm-with-vars)))

(defmethod toggle-fm-with-vars ((frames cons))
  (dolist (f frames) (eval f))
  *fm*)

;;; end-of-file
          
