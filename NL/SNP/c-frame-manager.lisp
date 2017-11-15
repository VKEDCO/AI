#|
===================================================================
-*- Mode: Lisp; Syntax: Common-Lisp; Package: SNP -*-

File: c-frame-manager.lisp
Author: Vladimir Kulyukin
Description: CLOS Frame System
1994, 1997-98, 2002

The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
====================================================================
|#

(eval-when (load eval compile)
  (unless (find-package "SNP")
    (make-package "SNP")))

(in-package  "SNP")
(use-package "USER")

(export '(make-frame-manager frame-and-attribute-to-inherited-value
                             define-frame-from-slot-list get-all-absts 
                             abst-and-slots-to-frame 
                             m-frame-to-slots abstp make-slot 
                             clear-frame-memory frame same-frame-p 
                             part-of-p whole-of-p 
                             abstp specp abst-or-whole-of-p 
                             spec-or-part-of-p))

(import '(user::deftable user::mappend))

(defvar *fm* nil "Global instance of the c-frame-manager class.")

;;; Each frame has the following parts:
;;; - a list of slots, i.e., attribute value pairs
;;; - a list of proper abstractions, i.e., frames directly above the current 
;;;   frame
;;; - a list of all abstractions, i.e., all frames above the current frame
;;; - a list of token sequences, i.e., input patterns that activate the frame

(deftable frame-slots)
(deftable frame-proper-absts)
(deftable frame-all-absts)
(deftable frame-specs)
(deftable frame-tokseqs)

(defclass c-frame-manager ()
  ((m-frame-to-slots         :initarg :m-frame-to-slots 
                             :initform *frame-slots-table*
                             :accessor m-frame-to-slots)
   (m-frame-to-proper-absts  :initarg :m-frame-to-proper-absts 
                             :initform *frame-proper-absts-table*
                             :accessor m-frame-to-proper-absts)
   (m-frame-to-all-absts     :initarg :m-frame-to-all-absts
                             :initform *frame-all-absts-table*
                             :accessor m-frame-to-all-absts)
   (m-frame-to-specs         :initarg :m-frame-to-specs
                             :initform *frame-specs-table*
                             :accessor m-frame-to-specs)
   (m-frame-to-tokseqs       :initarg :m-frame-to-tokseqs
                             :initform *frame-tokseqs-table*
                             :accessor m-frame-to-tokseqs)
   ))

(defun make-frame-manager () 
  (make-instance 'c-frame-manager))

(defclass c-slot ()
  ((m-attribute :initarg :m-attribute :accessor m-attribute)
   (m-value :initarg :m-value :initform nil :accessor m-value)))

(defmethod print-object ((this-slot c-slot) stream)
  (print-unreadable-object (this-slot stream :type t :identity t)
    (format stream "~S ~S" (m-attribute this-slot)
      (m-value this-slot))))

(defun make-slot (attr val)
  (make-instance 'c-slot :m-attribute attr :m-value val))

(defun make-slots (attr-val-list)
  (loop 
    for (attr val) in attr-val-list
    collect (make-slot attr val)))

(defun init-frame-manager (&key frames)
  (setf *fm* (make-frame-manager))
  (load frames)
  *fm*)

(defmethod toggle-frame-manager ((frames pathname))
  (init-frame-manager :frames frames))

(defmethod toggle-frame-manager ((frames string))
  (init-frame-manager :frames frames))

(defmethod toggle-frame-manager :before ((frames cons))
  (setf *fm* (make-frame-manager)))

(defmethod toggle-frame-manager ((frames cons))
  (dolist (f frames) (eval f))
  *fm*)

(defmethod get-slots ((fm c-frame-manager) (frame t))
  (multiple-value-bind (slots t-or-nil)
    (get-frame-slots frame (m-frame-to-slots fm))
    (if t-or-nil
	slots
      nil)))

(defmethod set-slots ((fm c-frame-manager) (frame t) (slots list))
  (setf (get-frame-slots frame (m-frame-to-slots fm)) 
    slots))

(defmethod add-slots ((fm c-frame-manager) (frame t) (slots list))
  (setf (get-frame-slots frame (m-frame-to-slots fm))
    (append slots (get-slots fm frame))))

(defmethod add-slot ((fm c-frame-manager) (frame t) (attr t) (val t))
  (multiple-value-bind (slots t-or-nil)
      (get-frame-slots frame (m-frame-to-slots fm))
    (if t-or-nil
        (push (make-slot attr val)
              (get-frame-slots frame (m-frame-to-slots fm)))
      (setf (get-frame-slots frame (m-frame-to-slots fm))
        (list (make-slot attr val))))))

(defmethod frame-and-attribute-to-slot ((fm c-frame-manager) 
                                        (frame t) 
                                        (attr t)
                                        &key (test #'eql))
  "Find slot with attribute attr in frame."
  (find attr (get-slots fm frame) :key #'m-attribute :test test))

(defmethod frame-and-attribute-to-value ((fm c-frame-manager)
                                         (frame t)
                                         (attr t)
                                         &key (test #'eql))
  "Find the value of attr in frame."
  (let ((slot (frame-and-attribute-to-slot fm frame attr :test test)))
    (if slot
	(m-value slot)
      nil)))

(defmethod frame-and-attribute-to-inherited-value ((fm c-frame-manager)
                                                   (frame t)
                                                   (attr t)
                                                   &key (test #'eql))
  "Find the inherited value of attr in frame if it exists."
  (let ((val (frame-and-attribute-to-value fm frame attr :test test)))
    (if val
	val
      (some #'(lambda (abst)
                (let ((val (frame-and-attribute-to-inherited-value 
                            fm abst attr :test test)))
                  (if val val nil)))
            (get-proper-absts fm frame)))))

(defmethod set-slot-value ((fm c-frame-manager) (frame t) 
                           (attr t) (val t) &key (test #'eql))
  "Set attribute attr's value to val in frame."
  (let ((slot (frame-and-attribute-to-slot fm frame attr :test test)))
    (if slot
        (setf (m-value slot) val)
      (add-slot fm frame attr val))))

(defmethod get-all-absts ((fm c-frame-manager) (frame t))
  "Get all abstractions of a frame."
  (multiple-value-bind (absts t-or-nil)
      (get-frame-all-absts frame (m-frame-to-all-absts fm))
    (if t-or-nil absts (list frame))))

(defmethod get-proper-absts ((fm c-frame-manager) (frame t))
  "Get proper abstractions of a frame."
  (multiple-value-bind (absts t-or-nil)
    (get-frame-proper-absts frame (m-frame-to-proper-absts fm))
    (if t-or-nil absts nil)))

(defmethod get-specs ((fm c-frame-manager) (frame t))
  "Get frame specifications."
  (multiple-value-bind (specs t-or-nil)
    (get-frame-specs frame (m-frame-to-specs fm))
    (if t-or-nil specs nil)))

(defmethod add-abst ((fm c-frame-manager) (spec-frame t) (abst-frame t)
                     &key (test #'eql))
  "Add a proper abstraction of a frame."
  (pushnew abst-frame 
           (get-frame-proper-absts spec-frame (m-frame-to-proper-absts fm))
           :test test))

(defmethod add-spec ((fm c-frame-manager) (abst-frame t) (spec-frame t)
                     &key (test #'eql))
  "Get a specification of a frame."
  (pushnew spec-frame 
           (get-frame-specs abst-frame (m-frame-to-specs fm))
           :test test)
  (pushnew abst-frame
           (get-frame-proper-absts spec-frame 
                                   (m-frame-to-proper-absts fm))
           :test test)
  (recompute-absts fm spec-frame))

(defmethod add-tokseq ((fm c-frame-manager) (frame t) (tokseq cons)
                       &key (test #'equal))
  "Add a token sequence to a frame."
  (pushnew tokseq 
           (get-frame-tokseqs frame (m-frame-to-tokseqs fm))
           :test test))

(defmethod get-tokseqs ((fm c-frame-manager) (frame t))
  "Get all token sequences associated with a frame."
  (multiple-value-bind (tokseqs bool)
      (get-frame-tokseqs frame (m-frame-to-tokseqs fm))
    (if bool tokseqs nil)))

(defmethod get-all-frames ((fm c-frame-manager))
  "Get a list of all frames."
  (let ((lst '()))
    (map-frame-slots #'(lambda (f slots)
                         (declare (ignore slots))
                         (push f lst))
                     (m-frame-to-slots fm))
    lst))

(defmethod recompute-specs ((fm c-frame-manager) (frame t)
                            &key (test #'eql))
  "Make sure that the frame is in the specs of each
of its proper abstractions."
  (loop for abst in (get-proper-absts fm frame)
        do (add-spec fm abst frame :test test)))
  
(defmethod recompute-absts ((fm c-frame-manager) (frame t))
  "Make sure that the frame is in the list of abstractions
of the frame is uptodate."
  (loop
    initially
    (setf (get-frame-all-absts frame (m-frame-to-all-absts fm)) 
      (compute-all-absts fm frame))
    for spec in (get-specs fm frame)
    do (recompute-absts fm spec)))

(defmethod compute-all-absts ((fm c-frame-manager) (frame t))
  "Compute all abstractions of the frame."
  (labels ((compute-all-absts-local (frame)
            (let ((absts (get-proper-absts fm frame)))
              (if (null absts)
                  '()
                `(,@absts ,@(mappend #'compute-all-absts-local absts))))))
    `(,frame ,@(remove-duplicates (compute-all-absts-local frame)))))

(defmethod abstp ((fm c-frame-manager) (abst-frame t) (spec-frame t)
                  &key (test #'eql))
  (if (find abst-frame (get-all-absts fm spec-frame) :test test)
      t
    nil))

(defmethod specp ((fm c-frame-manager) (spec-frame t) (abst-frame t))
  (abstp fm abst-frame spec-frame))

(defmethod part-of-p ((fm c-frame-manager) (part-frame t) (whole-frame t)
                      &key (test #'eql))
  (if (member part-frame (compute-all-slots fm whole-frame)
              :key #'m-value :test test)
      t
    nil))

(defmethod whole-of-p ((fm c-frame-manager) (whole-frame t) 
                       (part-frame t))
  (part-of-p fm whole-frame part-frame))

(defmethod abst-or-whole-of-p ((fm c-frame-manager) (big-frame t) (small-frame t))
  (or (abstp fm big-frame small-frame)
      (part-of-p fm small-frame big-frame)))

(defmethod spec-or-part-of-p ((fm c-frame-manager) (small-frame t)
                              (big-frame t))
  (or (specp fm small-frame big-frame)
      (part-of-p fm small-frame big-frame)))

(defmethod frame-symbol-p ((fm c-frame-manager) (frame t))
  (multiple-value-bind (val bool)
      (get-frame-slots frame (m-frame-to-slots fm))
    (declare (ignore val))
    bool))

(defmethod clear-frame-memory ((fm c-frame-manager))
  "Clears all frame info from the frame manager."
  (clear-frame-slots (m-frame-to-slots fm))
  (clear-frame-proper-absts (m-frame-to-proper-absts fm))
  (clear-frame-all-absts (m-frame-to-all-absts fm))
  (clear-frame-specs (m-frame-to-specs fm))
  (clear-frame-tokseqs (m-frame-to-tokseqs fm)))

(defmacro defframe (frame absts av-list)
  `(define-frame-from-attr-val-list ,*fm*
                                    :frame ',frame
                                    :absts ',absts
                                    :attr-val-list ',av-list
                                    ))

(defmethod define-frame-from-attr-val-list ((fm c-frame-manager) 
                                            &key frame (absts nil) 
                                            (attr-val-list nil))
  (setf (get-frame-proper-absts frame (m-frame-to-proper-absts fm))
    absts)
  (setf (get-frame-slots frame (m-frame-to-slots fm))
    (make-slots attr-val-list))
  (recompute-specs fm frame)
  (recompute-absts fm frame)
  frame)

(defmethod define-frame-from-slot-list ((fm c-frame-manager) &key frame (absts nil) (slots nil))
  (setf (get-frame-proper-absts frame (m-frame-to-proper-absts fm))
	absts)
  (setf (get-frame-slots frame (m-frame-to-slots fm))
	slots)
  (recompute-specs fm frame)
  (recompute-absts fm frame)
  frame)

;;; See if any proper abstractions contain an instance callback for the newly
;;; generated frame.
(defmethod define-frame-from-slot-list :after ((fm c-frame-manager) &key frame (absts nil) (slots nil))
  (declare (special *snp*))
  (loop
    for abst in (get-proper-absts fm frame)
    do (loop 
         for ic in (get-inst-callbacks abst)
         do (add-callback-to-frame *snp* frame ic))))

(defmethod compute-all-slots ((fm c-frame-manager) (frame t))
  (remove-duplicates
   `(,@(loop for abst in (get-proper-absts fm frame)
	     append (compute-all-slots fm abst))
       ,@(get-slots fm frame))
   :key #'m-attribute))

;;; This function is likely to change in the future.
;;; the specs chould be sorted by how many of their slots 
;;; abstract the given slots. First, you collect all frames 
;;; whose slots abstract the given slots, and then you compute 
;;; the slot-specialization-score for each of the specs found. 
;;; If all scores are 0, a new frame is defined.
(defmethod abst-and-slots-to-frame ((fm c-frame-manager) 
                                    (abst t) 
                                    (slots list))
  "Find a frame given an abstraction and slots."
  (if (null slots)
      abst
    (let* ((specs (specialize-frame fm abst slots))
	   (spec  (first specs)))
      (if (and (null (rest specs))
	       (slots-subset-p fm spec slots))
	  spec
	(define-frame-from-slot-list fm 
	  :frame (generate-frame-name spec) 
	  :absts specs
	  :slots slots)))))

(defmethod specialize-frame ((fm c-frame-manager) (abst t) (slots list))
  "Find specs of a frame given an abstraction and slots."
  (let* ((all-specs (loop for spec in (get-specs fm abst)
			  when (slots-abst-p fm spec slots)
			  append (specialize-frame fm spec slots)))
	 (specs (remove-duplicates all-specs)))
    (if (null specs) (list abst) specs)))

(defmethod slots-subset-p ((fm c-frame-manager) (abst-frame t) (slots list))
  "True if abst-frame's slots are a superset of slots."
  (subsetp slots (compute-all-slots fm abst-frame)
	   :test #'(lambda (slot1 slot2)
              (and (eql (m-attribute slot1)
                        (m-attribute slot2))
                   (eql (m-value slot1)
                        (m-value slot2))))))

;;; vk-21jan2000. This method will change to return a numerical score that computes 
;;; how many slots of abst-frame abstract the slots in the list
;;; when a attribute-value of abst-frame abstracts the corresponding
;;; attribute-value of a slot, add 1, else add 0.
;;; When this is done, the function will be renamed
;;; into slot-abstraction-score. This will allow for more
;;; flexible frame management.
(defmethod slots-abst-p ((fm c-frame-manager) (abst-frame t) (slots list))
  (every #'(lambda (slot)
             (with-slots (m-attribute m-value) slot
               (abstp fm
                      (frame-and-attribute-to-inherited-value
                       fm abst-frame m-attribute)
                      m-value)))
         slots))



;;; this method can be used only on instances.
;;; when the instance is removed, we do not have
;;; to recompute any abstractions.
(defmethod remove-instance-frame ((fm c-frame-manager) (frame t))
  (declare (special *snp*))
  (loop for abst in (get-proper-absts fm frame)
      do (setf (get-frame-specs abst (m-frame-to-specs fm))
           (delete frame (get-frame-specs (m-frame-to-specs fm)))))The software is provided "AS IS" without any express or implied
warranties under the creative commons license (https://creativecommons.org/licenses/).
  (rem-frame-slots frame (m-frame-to-slots fm))
  (rem-frame-proper-absts frame (m-frame-to-proper-absts fm))
  (rem-frame-all-absts frame (m-frame-to-all-absts fm))
  (rem-frame-specs frame (m-frame-to-specs fm))
  (rem-frame-tokseqs frame (m-frame-to-specs fm))
  (rem-callbacks (m-inst-callbacks *snp*))
  )

(defmethod same-frame-p ((fm c-frame-manager) (frame1 t) (frame2 t))
  (declare (ignore fm))
  (eq frame1 frame2))

(defun generate-frame-name (frame)
  (intern (gentemp (format nil ":~A-" frame))
          :SNP))



;;; end-of-file
