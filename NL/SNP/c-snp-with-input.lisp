#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: c-snp-with-input.lisp
Author: Vladimir Kulyukin
Description: A sublclass of the semantic network processor that
             keeps track of the input and counts input tokens.
2001

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#

(eval-when (load eval compile)
  (unless (find-package "SNP")
    (make-package "SNP")))

(in-package  "SNP")
(use-package "USER")

(defclass c-snp-with-input (c-snp)
  ((m-input :initform nil :accessor m-input :initarg :m-input)
   (m-token-count :initform 0 :accessor m-token-count :initarg :m-token-count)))


(defmethod process-tokens ((this-snp c-snp-with-input) (tokens list)
                           &key (test #'eql))
  "Spread actication in the network from the list of input tokens."
  (loop
    initially
    (clear-referenced-frames this-snp)
    (setf (m-token-count this-snp) 0)
    (setf (m-input this-snp) tokens)
    for tok in tokens
    do
    (incf (m-token-count this-snp))
    (spread-activation this-snp tok :test test)))

(defmethod process-tokens ((this-snp c-snp-with-input) (tokens string)
                           &key (test #'eql))
  (let ((symbol-tokens (to-symbols tokens :package (find-package "SNP"))))
    (process-tokens this-snp symbol-tokens :test test)))

(defun space-punctuation (str)
  (substitute-if-not #\space #'(lambda (ch)
                                 (or (char= ch #\space)
                                     (alphanumericp ch)))
                     str))

(defun string-to-symbols (token-string)

  (loop
    with clean-str = (space-punctuation str)
    with curr-pos = 0
    while t
    do (multiple-value-bind (sym next-pos)
           (read-from-string clean-str nil :eos :start curr-pos)
         (if (eql sym :eos)
             (return ht)
           (progn
             (update-freq-table (write-to-string sym) ht 1)
             (setf curr-pos next-pos))))))


(defun toggle-snp-with-input (&key frames tokseqs)
  (clear-snp *snp*)
  (setf *snp* (make-instance 'c-snp-with-input))
  (setf (m-frame-manager *snp*)
    (toggle-frame-manager frames))
  (install-tokseqs *snp* tokseqs)
  *snp*)

;;; vk-18jun01: this method forms a basis of elementary
;;; language acquisition.
(defmethod get-unknown-tokens ((this-snp c-snp-with-input) (frame t))
  "The input tokens can be split into two parts: known
and unknown. The known tokens are those that are specified in
a token sequence associated with the frame. The unknown tokens
are the tokens that are not specified. By definition, the
known tokens precede the unknown tokens. This method returns
the unknown tokens."
  (with-slots (m-frame-manager) this-snp
    (loop
      with tokseqs = (get-frame-tokseqs frame
                                        (m-frame-to-tokseqs m-frame-manager))
      with input = (m-input this-snp)
      with tc = (m-token-count this-snp)
      for tokseq in tokseqs
      when (equal (subseq input 0 tc) tokseq)
      do (return (subseq input tc)))))

;;; end-of-file
