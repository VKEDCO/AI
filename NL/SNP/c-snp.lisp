#|
========================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: c-snp.lisp
Author: Vladimir Kulyukin
Description: A semantic network processor class that
             implements a DMAP-style spreading activation
             algorithm on semantic networks of frames where
             frames are connected via abstraction and packaging
             relationships.
1994, 1996, 1998, 2000, 2001

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
========================
|#

(eval-when (load eval compile)
  (unless (find-package "SNP")
    (make-package "SNP")))

(in-package  "SNP")
(use-package "USER")

(export '(process-tokens make-snp))
(import '(user::deftable user::variable-p))

(defvar *snp* nil "A instance of the semantic network processor.")

(deftable static-expectations)
(deftable dynamic-expectations)
(deftable callbacks)
(deftable inst-callbacks)

(defclass c-expectation ()
  ((m-frame    :initarg :m-frame    :initform nil :accessor m-frame)
   (m-tok-seq  :initarg :m-tok-seq  :initform nil :accessor m-tok-seq)
   (m-slots    :initarg :m-slots    :initform nil :accessor m-slots)))

(defclass c-static-expectation  (c-expectation) ())

(defclass c-dynamic-expectation (c-expectation) 
  ((m-init-frame :initarg :m-init-frame :initform nil :accessor m-init-frame)
   (m-active-p   :initarg :m-active-p :initform t :accessor m-active-p)
   (m-current-token :initarg :m-current-token :initform nil :accessor m-current-token)))

(defmethod print-object ((exp c-static-expectation) stream)
  "Print a static expectation into a stream."
  (with-slots (m-frame m-tok-seq m-slots) exp
    (print-unreadable-object
       (exp stream :type t :identity t)
      (format stream "~S ~S ~S" m-frame m-tok-seq m-slots))))

(defmethod print-object ((exp c-dynamic-expectation) stream)
  "Print a dynamic expectation into a stream."
  (with-slots (m-frame m-tok-seq m-slots m-init-frame m-current-token m-active-p) exp
    (print-unreadable-object
        (exp stream :type t :identity t)
      (format stream "~S ~S ~S ~S ~S ~S" m-frame m-tok-seq m-slots 
        m-init-frame m-current-token m-active-p))))

(defun make-static-expectation (&key frame tok-seq slots)
  "Construct a static expectation."
  (make-instance 'c-static-expectation
    :m-frame frame :m-tok-seq tok-seq :m-slots slots))

(defun make-dynamic-expectation (&key frame tok-seq slots init-frame current-token)
  "Construct a dynamic expectation."
  (make-instance 'c-dynamic-expectation
    :m-frame frame 
    :m-tok-seq tok-seq
    :m-slots slots
    :m-init-frame init-frame
    :m-current-token current-token
    :m-active-p t))

;;; semantic network processor class
(defclass c-snp ()
  ((m-frame-manager         :initarg :m-frame-manager 
                            :accessor m-frame-manager)
   (m-static-expectations   :initarg :m-static-expectations 
                            :initform *static-expectations-table*
                            :accessor m-static-expectations)
   (m-dynamic-expectations  :initarg :m-dynamic-expectations 
                            :initform *dynamic-expectations-table*
                            :accessor m-dynamic-expectations)
   (m-callbacks             :initarg :m-callbacks 
                            :initform *callbacks-table*
                            :accessor m-callbacks)
   (m-inst-callbacks        :initarg :m-inst-callbacks
                            :initform *inst-callbacks-table*
                            :accessor m-inst-callbacks)
   (m-referenced-frames     :initarg :m-referenced-frames
                            :initform nil
                            :accessor m-referenced-frames)
   ))

;;; To make snp, one needs to provide a frame manager
(defun make-snp (&key frame-manager)
  "Construct a semantic network processor instance."
  (make-instance 'c-snp :m-frame-manager frame-manager))

(defmethod head-token ((exp c-expectation))
  "Head token is the first token of the expectation's
token sequence."
  (first (m-tok-seq exp)))

(defmethod add-expectation ((this-snp c-snp) (exp c-dynamic-expectation))
  "Add a dynamic expectation to the semantic network processor."
  (with-slots (m-dynamic-expectations) this-snp
    (push exp 
          (get-dynamic-expectations (expected-token this-snp exp)
                                    m-dynamic-expectations))))

(defmethod add-expectation ((this-snp c-snp) (exp c-static-expectation))
  "Add a static expectation to the semantic network processor."
  (with-slots (m-static-expectations) this-snp
    (push exp
          (get-static-expectations (expected-token this-snp exp)
                                   m-static-expectations))))

(defmacro deftokseq (frame &rest tok-seq)
  "Associate a token sequence with a frame. Note
that *snp* must be bound to an instance of the
semantic network processor."
  `(define-token-seq ,*snp* ',frame ',tok-seq))

(defmethod define-token-seq ((this-snp c-snp) 
                             (frame t) 
                             (tok-seq list)
                             &key (test #'eql))
  (add-tokseq (m-frame-manager this-snp) frame tok-seq)
  (unless (and (funcall test frame (first tok-seq))
               (null (rest tok-seq)))
    (let ((exp (make-static-expectation :frame frame
                                        :tok-seq tok-seq)))
      (add-expectation this-snp exp))))
                                              
(defmethod expected-token ((this-snp c-snp) (exp c-expectation))
  "Compute the token that exp must wait for."
  (with-slots (m-frame-manager) this-snp
    (with-slots (m-frame m-tok-seq) exp
      (let ((tok (first m-tok-seq)))
        (if (attribute-specifier-p tok)
            (let ((val (frame-and-attribute-to-inherited-value
                        m-frame-manager m-frame tok)))
              (if val
                  val
                (error "~S is not an attribute in ~S" tok m-frame)))
          tok)))))

(defun attribute-specifier-p (x)
  "Attribite is preceded with :=="
  (and (keywordp x)
       (let ((sn (symbol-name x)))
         (and (char= (char sn 0) #\=)
              (char= (char sn 1) #\=)))))

(defmethod expectations-on-token ((this-snp c-snp) (tok t))
  "Get all expectations waiting for the token tok."
  (with-slots (m-static-expectations m-dynamic-expectations) this-snp
    `(,@(get-static-expectations tok m-static-expectations)
      ,@(filter-dynamic-expectations
         (get-dynamic-expectations tok m-dynamic-expectations)))))

(defun filter-dynamic-expectations (dyn-exps)
  "Expectations that are no longer active do not compete for input."
  (remove-if-not #'(lambda (exp) (m-active-p exp)) dyn-exps))

(defmethod process-tokens ((this-snp c-snp) (tokens list)
                           &key (test #'eql))
  "Spread actication in the network from the list of 
input tokens."
  (loop
    initially (clear-referenced-frames this-snp)
    for tok in tokens
    do (spread-activation this-snp tok :test test)))

(defmethod spread-activation :before ((this-snp c-snp) (tok t)
                                    &key (test #'eql))
  "If a token is a frame, add it to the list of
referenced frames. This is done for bookkeeping only."
   (add-referenced-frame this-snp tok :test test))

(defmethod add-referenced-frame :around ((this-snp c-snp) (frame t)
                                         &key (test #'eql))
  "A frame is referenced iff frame is the name of a real frame."
  (if (frame-symbol-p (m-frame-manager this-snp) frame)
      (call-next-method)
    nil))

(defmethod add-referenced-frame ((this-snp c-snp) (frame t) &key (test #'eql))
  "Add a frame to the list of referenced frames. This is useful
for debugging and analysis."
  (with-slots (m-referenced-frames) this-snp
    (pushnew frame m-referenced-frames :test test)))

(defmethod clear-referenced-frames ((this-snp c-snp))
  "Clear the list of referenced frames."
  (setf (m-referenced-frames this-snp) nil))

(defmethod spread-activation ((this-snp c-snp) (tok t)
                              &key (test #'eql))
  "Spread activation on the token tok."
  (with-slots (m-frame-manager) this-snp
    (dolist (abst (get-all-absts m-frame-manager tok))
      (dolist (exp (expectations-on-token this-snp abst))
        (advance-expectation this-snp exp tok :test test)))))

(defmethod advance-expectation ((this-snp c-snp) 
                                (exp c-static-expectation) 
                                (tok t)
                                &key (test #'eql))
  "Advance a static expectation on a given token."
  (with-slots (m-frame-manager) this-snp
    (with-slots (m-frame m-tok-seq m-slots) exp
      (let ((cur-tok (first m-tok-seq))
            (advanced-tok-seq (rest m-tok-seq))
            (new-slots (extend-slots this-snp exp tok)))
        (if (null advanced-tok-seq)
            (let ((spec-frame (abst-and-slots-to-frame
                               m-frame-manager m-frame new-slots)))
              (run-callbacks this-snp spec-frame)
              (spread-activation this-snp spec-frame :test test))
          (unless (dynamic-expectation-initiated-p this-snp cur-tok
                                                   advanced-tok-seq)
            (let ((dexp (make-dynamic-expectation :frame m-frame
                                                  :tok-seq advanced-tok-seq
                                                  :slots new-slots
                                                  :current-token tok
                                                  :init-frame tok)))
              ;;; Deactivate all expectations that wait for a non-frame
              ;;; token different from the non-frame token tok.
              (deactivate-expectations-misfired-on-token this-snp tok :test test)
              (add-expectation this-snp dexp))))))))

;;; vk-20mar98: One cannot advance an expectation on the frame 
;;; that it wants to reference. This puts a restriction on
;;; the structure of token sequences associated with
;;; a frame. Specifically, they cannot be recursive, i.e.,
;;; reference the frame that they are associated with.
;;; Hence, the test (same-frame-p frame-manager frame tok)
;;; This is done to ensure that the network converges
;;; on any input. This may be undesirable for some applications,
;;; in which case the restriction must be removed. The tradeoff is,
;;; as usual, clarity vs. flexibility. Recursive token sequences
;;; give the knowledge engineer a lot more flexibility. However,
;;; the spreading activation process becomes much harder to
;;; reason about.

(defmethod advance-expectation ((this-snp c-snp) 
                                (exp c-dynamic-expectation) 
                                (tok t)
                                &key (test #'eql))
  "Advance a dynamic expectation on a given token."
  (with-slots (m-frame-manager) this-snp
    (with-slots (m-frame m-tok-seq m-slots m-init-frame) exp
        (unless (same-frame-p m-frame-manager m-frame tok)
          (let ((advanced-tok-seq (rest m-tok-seq))
                (new-slots (extend-slots this-snp exp tok)))
            (deactivate-expectation this-snp exp tok)
            (if (null advanced-tok-seq)
                (let ((spec-frame (abst-and-slots-to-frame
                                   m-frame-manager m-frame new-slots)))
                  (unless (funcall test spec-frame m-frame)
                    (run-callbacks this-snp m-frame))
                  (run-callbacks this-snp spec-frame)
                  (deactivate-misfired-expectations this-snp exp tok :test test)
                  (deactivate-rival-expectations this-snp exp :test test)
                  (spread-activation this-snp spec-frame :test test))
              (let ((dexp (make-dynamic-expectation 
                           :frame m-frame
                           :tok-seq advanced-tok-seq
                           :slots new-slots
                           :current-token tok
                           :init-frame m-init-frame)))
                ;;; deactivate unless they were advanced on the same token
                ;;; that you were
                (deactivate-misfired-expectations this-snp exp tok :test test)
                (add-expectation this-snp dexp))))))))

(defmethod dynamic-expectation-initiated-p ((this-snp c-snp) 
                                           (tok t) 
                                           (tokens null))
  (declare (ignore tp) (ignore tok) (ignore tok-seq))
  nil)


(defmethod dynamic-expectation-initiated-p ((this-snp c-snp) 
                                            (possible-initiator-tok t)
                                            (tokens cons))
  "Check if there is a dynamic-expectation that was initiated by an 
abstraction of initiator-tok and that is waiting on the same tokens."
  (let ((next-tok (first tokens)))
    (with-slots (m-dynamic-expectations m-frame-manager) this-snp
      (some #'(lambda (dexp)
                (with-slots (m-tok-seq m-init-frame m-active-p) dexp
                  (and m-active-p
                       (equal m-tok-seq tokens)
                       (abstp m-frame-manager m-init-frame
                              possible-initiator-tok))))
            (get-dynamic-expectations next-tok m-dynamic-expectations)))))
  
(defmethod run-callbacks ((this-snp c-snp) (frame t))
  (with-slots (m-callbacks) this-snp
    (dolist (clb (get-callbacks frame m-callbacks))
      (funcall clb frame))))

(defmethod deactivate-expectation ((this-snp c-snp)
                                   (exp c-dynamic-expectation)
                                   (tok t)
                                   &key (delete-p nil))
  "If delete-p flag is t, the expectation is physically
removed from the list of expectations keyed on tok. This
should be done when efficiency becomes an issue. If delete-p
is nil, which is the default, the expectation is deactivated
by remains on the list. This is useful for debugging."
  (if delete-p
      (with-slots (m-dynamic-expectations) this-snp
        (setf (get-dynamic-expectations tok m-dynamic-expectations)
          (delete exp (get-dynamic-expectations tok m-dynamic-expectations)))
        (when (null (get-dynamic-expectations tok m-dynamic-expectations))
          (rem-dynamic-expectations tok m-dynamic-expectations)))
    (setf (m-active-p exp) nil)))

(defmethod deactivate-rival-expectations ((this-snp c-snp)
                                          (exp c-dynamic-expectation)
                                          &key (test #'eql))
  "Deactivate all expectations that compete for the same
frame but have failed to complete."
  (map-dynamic-expectations
   #'(lambda (key exp-list)
       (loop
         with target-frame = (m-frame exp)
         for e in exp-list
         when (and (not (eq e exp))
                   (m-active-p e)
                   (funcall test (m-frame e) target-frame))
         do (deactivate-expectation this-snp e key)))
   (m-dynamic-expectations this-snp)))

;;; vk-20jun00. Here is the reasoning behind determining whether an 
;;; expectation has misfired. Given an expectation e1 and a token tok, 
;;; another expectation e2 is considered misfired iff
;;; - e2's target frame is the same frame as e1's
;;; - e2's init-frame is the same as e1's
;;; - there is no expectation e3 waiting for tok s.t. e3's target
;;;   is e2's expected token.
;;; In other words, if e2's expected token begins with tok,
;;; e2 cannot be deactivated.
;;; To compute the third condition, get all exps keyed on tok
;;; and see if their targets are different from e2's
;;; expected token.

(defmethod expectation-misfired-p ((this-snp c-snp) 
                                   (e1 c-dynamic-expectation)
                                   (e2 c-dynamic-expectation)
                                   (tok t)
                                   &key (test #'eql))
  (and (m-active-p e2)
       (funcall test (m-frame e1) (m-frame e2))
       (funcall test (m-init-frame e1) (m-init-frame e2))
       (funcall test (m-current-token e1) (m-current-token e2))
       (not (conflict-expectation-exists-p this-snp 
                                           (expected-token this-snp e2) 
                                           tok))))

(defmethod conflict-expectation-exists-p ((this-snp c-snp) (exp-token t) (tok t)
                                           &key (test #'eql))
  (some #'(lambda (dexp)
            (and (m-active-p dexp) (funcall test (m-frame dexp) exp-token)))
     (get-dynamic-expectations tok (m-dynamic-expectations this-snp))))

(defmethod deactivate-misfired-expectations ((this-snp c-snp)
                                             (exp c-dynamic-expectation)
                                             (tok t)
                                             &key (test #'eql))
  "Deactivate all frames that expected a token other than
tok or have advanced on tok on the current iteration. Do not deactivate 
any frames that expect other frames, because activation of frames may take 
several tokens. Also if tok is a frame it cannot deactivate expectations 
that wait for non-frame tokens."
  (with-slots (m-frame-manager) this-snp
    (map-dynamic-expectations
     #'(lambda (key exp-list)
         (unless (funcall test key tok)
           (if (frame-symbol-p m-frame-manager key)
               (loop
                 ;initially (format t "~%Deactivating exps keyed on frame ~A~%" key)
                 for e in exp-list
                 when (expectation-misfired-p this-snp exp e tok)
                 do
                 ;(format t "~%Deactivating ~A~%" e)
                 (deactivate-expectation this-snp e key))
             (if (not (frame-symbol-p m-frame-manager tok))
                 (loop 
                   ;initially (format t "~%Deactivating exps keyed on token ~A" key)
                   for e in exp-list
                   ;;; do not deactivate if the expectation has been advanced
                   ;;; on the current token.
                   unless (funcall test (m-current-token e) tok)
                   do
                   ;(format t "~%Deactivating ~A~%" e)
                   (deactivate-expectation this-snp e key))
               nil))))
     (m-dynamic-expectations this-snp))))

(defmethod deactivate-expectations-misfired-on-token :around ((this-snp c-snp)
                                                              (tok t)
                                                              &key (test #'eql))
  "Deactivate all expectations that waited for a non-frame token different
from the non-frame token tok."
  (unless (frame-symbol-p (m-frame-manager this-snp) tok)
    (call-next-method)))

(defmethod deactivate-expectations-misfired-on-token ((this-snp c-snp)
                                                      (tok t)
                                                      &key (test #'eql))
  "Deactivate all expectations that waited for a non-frame token
different from the non-frame token tok, unless their current token
is the same as tok. The latter case means that they have been activated
on the same iteration by the same token."
  (with-slots (m-frame-manager) this-snp
    (map-dynamic-expectations
     #'(lambda (key exp-list)
         (unless (or (funcall test key tok) 
                     (frame-symbol-p m-frame-manager key))
           (loop
             for e in exp-list
             unless (funcall test (m-current-token e) tok)
             do
             (deactivate-expectation this-snp e key))))
     (m-dynamic-expectations this-snp))))

(defmethod extend-slots ((this-snp c-snp) (exp c-expectation) (token t))
  (with-slots (m-frame-manager) this-snp
    (with-slots (m-token-seq m-slots) exp
      (let ((curr-tok (head-token exp)))
        (if (attribute-specifier-p curr-tok)
            (if (abstp m-frame-manager 
                       token 
                       (expected-token this-snp exp))
                m-slots
              (cons (make-slot curr-tok token)
                    m-slots))
          m-slots)))))

(defun toggle-snp (&key frames tokseqs)
  (setf *snp* (make-snp 
               :frame-manager
               (toggle-frame-manager frames)))
  (install-tokseqs *snp* tokseqs)
  *snp*)

(defun re-toggle-snp (&key frames tokseqs)
  (setf (m-frame-manager *snp*)
    (toggle-frame-manager frames))
  (install-tokseqs *snp* tokseqs)
  *snp*)


(defmethod install-tokseqs ((this-snp c-snp) (tokseqs pathname))
  (load tokseqs))

(defmethod install-tokseqs ((this-snp c-snp) (tokseqs string))
  (load tokseqs))

(defmethod install-tokseqs ((this-snp c-snp) (tokseqs cons))
  (dolist (tokseq tokseqs)
    (eval tokseq)))

                                                      
;;; ============= Tools ===================

(defmethod dbg-process-tokens ((this-snp c-snp)
                               (tokens cons))
  (process-tokens this-snp tokens))

(defmethod dbg-process-tokens :after ((this-snp c-snp)
                                      (tokens cons))
  (clear-expectations this-snp :dynamic))

(setf (symbol-function 'dbgpt) #'dbg-process-tokens)

(defmethod clear-frame-memory ((this-snp c-snp))
  (with-slots (m-frame-manager) this-snp
    (clear-frame-memory frame-manager)))

(defmethod clear-expectations ((this-snp c-snp) &optional (type :dynamic))
  (ecase type
    (:dynamic (clear-dynamic-expectations (m-dynamic-expectations this-snp)))
    (:static  (clear-static-expectations  (m-static-expectations  this-snp)))
    (:all     (clear-dynamic-expectations (m-dynamic-expectations this-snp))
              (clear-static-expectations  (m-static-expectations  this-snp)))))

(defmethod remove-callbacks ((this-snp c-snp))
  (with-slots (m-callbacks m-inst-callbacks) this-snp
    (clear-callbacks m-callbacks)
    (clear-inst-callbacks m-inst-callbacks)))

(defmethod clear-snp ((this-snp c-snp))
  (clear-frame-memory (m-frame-manager this-snp))
  (clear-expectations this-snp :all)
  (remove-callbacks this-snp)
  t)

(defmethod clear-snp ((this-snp null))
  nil)

(defmethod add-default-callbacks ((this-snp c-snp))
  (with-slots (m-callbacks m-frame-manager) this-snp
    (with-slots (m-frame-to-slots) m-frame-manager
      (map-callbacks #'(lambda (key val)
                         (declare (ignore val))
                         (push #'(lambda (tok)
                                   (declare (ignore tok))
                                   (format t "~A referenced...~%" key))
                               (get-callbacks key m-callbacks)))
                     m-frame-to-slots)
      t)))

(defmethod add-callback ((this-snp c-snp) (callback function))
  (with-slots (m-callbacks m-frame-manager) this-snp
    (with-slots (m-frame-to-slots) m-frame-manager
      (map-callbacks #'(lambda (key val)
                         (declare (ignore val))
                         (push callback (get-callbacks key callbacks)))
                     m-frame-to-slots))))

(defmethod add-callback-to-frame ((this-snp c-snp) (frame symbol) (callback function))
   (with-slots (m-callbacks m-frame-manager) this-snp
     (when (frame-symbol-p m-frame-manager frame)
       (push callback (get-callbacks frame m-callbacks)))))

(defmethod add-inst-callback-to-frame ((this-snp c-snp) (frame symbol) (callback function))
  (with-slots (m-inst-callbacks m-frame-manager) this-snp
    (when (frame-symbol-p m-frame-manager frame)
      (push callback (get-inst-callbacks frame m-inst-callbacks)))))

(defmethod print-expectations ((this-snp c-snp) 
                               &optional (type :dynamic)
                                         (stream t))
  (labels ((displayer (key val)
             (format stream "~S --> ~S~%" key val)))
    (ecase type
      (:dynamic (map-dynamic-expectations #'displayer
                                          (m-dynamic-expectations this-snp)))
      (:static (map-static-expectations #'displayer
                                        (m-static-expectations this-snp))))))

(defmethod check-dynamic-frame-count ((this-snp c-snp) (tok t) n)
  (let ((count 0))
    (with-slots (m-dynamic-expectations) this-snp
      (maphash #'(lambda (key exps)
                   (declare (ignore key))
                   (when (>= (count tok exps :key #'frame)
                             n)
                         (break)))
               m-dynamic-expectations))))

(defmethod print-frames ((this-snp c-snp) &optional (stream t))
  (with-slots (m-frame-manager) this-snp
    (with-slots (m-frame-to-slots) m-frame-manager
       (maphash #'(lambda (frame slots)
                    (format stream "[ ~S <--> ~A ]~%" frame slots))
                m-frame-to-slots))))

;;; end-of-file
