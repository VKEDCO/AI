#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: sample-var-tokseqs.lisp
Author: Vladimir Kulyukin
Description: Token sequences with variables that activate frames when
             seen in the input.
2000

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#

(in-package "SNP")

(defun primitive-symbol-token-p (x)
  (and (symbolp x)
       (not (frame-symbol-p (m-frame-manager *snp*)
                            x))))

(deftokseq m-number (?x . numberp))
(deftokseq m-agent-position :==x-coord :==y-coord)
(deftokseq m-number-triple (?x . numberp) (?y . numberp) (?z . numberp))
(deftokseq m-number-symbol-tuple (?x . numberp) (?y . primitive-symbol-token-p))

;;; end-of-file
