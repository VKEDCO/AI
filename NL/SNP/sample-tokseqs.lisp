#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: sample-tokseqs.lisp
Author: Vladimir Kulyukin
Description: Token sequences that activate frames when
             seen in the input.
1994, 1998

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#


(in-package "SNP")

(deftokseq m-dog dog)
(deftokseq m-dog a dog)
(deftokseq m-dog dogs)
(deftokseq m-human human)
(deftokseq m-human person)
(deftokseq m-wise-fido wise fido)
(deftokseq m-john john)
(deftokseq m-mary mary)
(deftokseq m-bone a bone)
(deftokseq m-apple an apple)
(deftokseq m-ptrans :==actor gave :==object to :==recipient)
(deftokseq m-ptrans :==actor gave :==recipient :==object)

;;; end-of-file
