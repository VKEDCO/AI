#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: sample-var-frames.lisp
Author: Vladimir Kulyukin
Description: A simple semantic network of frames.
2000

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#

(in-package "SNP")

(defframe m-number (m-root) ())
(defframe m-number-triple (m-root) ())
(defframe m-number-symbol-tuple (m-root) ())
(defframe m-agent-position (m-root)
          ((:==x-coord m-number)
           (:==y-coord m-number)))

;;; end-of-file
