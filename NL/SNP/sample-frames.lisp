#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: sample-frames.lisp
Author: Vladimir Kulyukin
Description: A simple semantic network of frames.
1994, 1998

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#

(in-package "SNP")

(defframe m-root () ())
(defframe m-agent (m-root) ())
(defframe m-physical-object (:m-root) ())
(defframe m-animal (m-root) ())
(defframe m-dog   (m-agent m-animal) ())
(defframe m-human (m-agent m-animal) ())
(defframe m-wise-fido (m-dog) ())
(defframe m-john (m-human) ())
(defframe m-mary (m-human) ())
(defframe m-bone (m-physical-object) ())
(defframe m-apple (m-physical-object) ())

(defframe m-ptrans (m-event)
  ((:==actor     m-agent)
   (:==object    m-physical-object)
   (:==recipient m-agent)))

;;; end-of-file
