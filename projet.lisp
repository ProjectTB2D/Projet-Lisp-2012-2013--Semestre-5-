
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projet de Lisp : Jordan Aupetit - Fabien Berarde - Mickaël Lemasson ;;
;;           Groupe A1 - Licence 3 Informatique - 2012/2013	       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass abstract-enumerator () ())

(defgeneric init-enumerator (enumerator)
  (:documentation
   "reinitializes and returns ENUMERATOR"))

(defgeneric copy-enumerator (enumerator)
  (:documentation
   "returns a reinitialized
copy of ENUMERATOR"))

(defgeneric next-element-p (enumerator)
  (:documentation
   "returns NIL if there is no next
element, a non NIL value otherwise"))

(defgeneric next-element (enumerator)
  (:documentation
   "returns the next element,
moves to the following one"))

(defgeneric call-enumerator (enumerator)
  (:documentation
   "if there is a next element e,
returns e and T
and moves to the next element;
otherwise returns NIL and NIL"))

(defgeneric unset-memo-object (enumerator)
  (:documentation
   "Desalloue le creneau qui sera affiché
a chaque call-enumerator"))


;; ======== DEFCLASS  ========

(defclass list-enumerator (abstract-enumerator)
  ((listEnum :initarg :listEnum :accessor listEnum)
   (saveListEnum :initarg :listEnum :reader saveListEnum)))

(defclass induction-enumerator (list-enumerator)
  ((functionF :initarg :functionF :reader functionF)
   (functionG :initarg :functionG :reader functionG)))

(defclass parallel-enumerator (list-enumerator)
  ())

(defclass filter-enumerator (list-enumerator)
  ((predicatEnum :initarg :predicatEnum :reader predicatEnum)))

(defclass append-enumerator (list-enumerator)
  ((save :initarg :save :accessor save)))

(defclass memo-enumerator (list-enumerator)
  ((memo :accessor memo)))

;; ===========================


(defmethod init-enumerator ((e abstract-enumerator))
  e)

(defmethod init-enumerator :after ((e abstract-enumerator))
  (setf (listEnum e) (saveListEnum e)))

(defmethod call-enumerator ((e abstract-enumerator))
  (if (next-element-p e)
      (values (next-element e) t)
      (values nil nil)))


;; ==== LIST ====

(defun make-list-enumerator (l &optional (circ NIL))
  (let ((e (make-instance 'list-enumerator :listEnum l)))
    (if circ
	(and (setf *print-circle* (listEnum e)) (setf (cdr (last (listEnum e))) (listEnum e))))
    e))

(defmethod copy-enumerator ((e list-enumerator)) 
  (make-list-enumerator (saveListEnum e)))

(defmethod next-element-p ((e list-enumerator))
  (not (endp (listEnum e))))

(defmethod next-element ((e list-enumerator))
  (pop (listEnum e)))


;; ==== INDUCTION ====

(defun make-mod-inductive-enumerator (i f &optional (g #'identity))
  (make-instance 'induction-enumerator :listEnum i :functionF f :functionG g))

(defmethod copy-enumerator ((e induction-enumerator)) 
  (make-mod-inductive-enumerator (saveListEnum e) (functionF e) (functionG e)))

(defmethod next-element-p ((e induction-enumerator))
  T)

(defmethod next-element ((e induction-enumerator))
  (let ((r (listEnum e)))
    (and (setf (listEnum e) (funcall (functionF e) (listEnum e)))
	 (setf (listEnum e) (funcall (functionG e) (listEnum e)))
	 r)))


;; ==== PARALLEL ====

(defmethod init-enumerator :after ((e parallel-enumerator))
  (mapcar #'init-enumerator (listEnum e)))

(defun make-parallel-enumerator (e &rest l)
  (make-instance 'parallel-enumerator :listEnum (mapcar #'copy-enumerator (cons e l))))

(defmethod copy-enumerator ((e parallel-enumerator))
  (make-parallel-enumerator (car (saveListEnum e)) (cdr (saveListEnum e))))

(defmethod next-element-p ((e parallel-enumerator))
  (not (some #'null (mapcar #'next-element-p (listEnum e)))))

(defmethod next-element ((e parallel-enumerator))
  (if (some #'null (mapcar #'next-element-p (listEnum e)))   
      nil
      (mapcar #'call-enumerator (listEnum e))))


;; ==== FILTER ====

(defun make-filter-enumerator (e p)
  (make-instance 'filter-enumerator :listEnum (copy-enumerator e) :predicatEnum p))

(defmethod copy-enumerator ((e filter-enumerator)) 
  (make-filter-enumerator (saveListEnum e) (predicatEnum e)))

(defmethod next-element-p ((e filter-enumerator)) 
  (labels ((next-e (predicat liste)
	     (if (listp liste)
		 (if (endp liste)
		     NIL
		     (if (funcall predicat (car liste))
			 T
			 (next-e predicat (cdr liste))))
		 T)))
    (next-e (predicatEnum e) (listEnum (listEnum e)))))

(defmethod next-element ((e filter-enumerator))
  (loop
    (multiple-value-bind (ret1 ret2) (call-enumerator (listEnum e))
      (if (null ret2)
	  (return ret2)
	  (when (funcall (predicatEnum e) ret1) 
	    (return ret1))))))


;; ==== CONCATENER ====

(defun make-append-enumerator (e)
  (make-instance 'append-enumerator :listEnum (listEnum e)))

(defmethod copy-enumerator ((e append-enumerator)) 
  (make-append-enumerator (make-list-enumerator (saveListEnum e))))

(defmethod next-element-p ((e append-enumerator))
  (not (endp (listEnum e))))
 
(defmethod next-element :before ((e append-enumerator))
  (labels 
      ((popEnum (l)
	 (if (atom (car l))
	     (car l)
	     (popEnum (car l))))

       (popEnum2 (l1 L2)
	 (if (atom l1)	     
	     l2
	     (if (endp l2)
		 (if (endp (cdr l1))
		     (popEnum2 (car l1) l2)
		     (popEnum2 (car l1) (append (cdr l1) l2)))
		 (if (endp (cdr l1))
		     (popEnum2 (car l1) l2)
		     (popEnum2 (car l1) (append (list (cdr l1)) l2)))))))
    
    (progn (setf (save e) (popEnum (listEnum e)))
	   (setf (listEnum e) (popEnum2 (listEnum e) '())))))

(defmethod next-element  ((e append-enumerator))
  (save e))


;; ===== MEMO ====

(defun make-memo-enumerator (e)
  (make-instance 'memo-enumerator :listEnum (copy-enumerator e)))

(defmethod copy-enumerator ((e memo-enumerator)) 
  (make-memo-enumerator (saveListEnum e)))

(defmethod next-element-p ((e memo-enumerator))
  (if (slot-boundp e 'memo)
      T
      (next-element-p (listEnum e))))
      
(defmethod next-element ((e memo-enumerator))
  (if (slot-boundp e 'memo)
      (memo e)
      (setf (memo e) (next-element (listEnum e)))))

(defmethod unset-memo-object ((e memo-enumerator))
  (slot-makunbound e 'memo))

						     
;; ============= TEST =============

(defparameter *e0* (make-list-enumerator '(1 ((nil)) (5 7))))
(defparameter *e1* (make-list-enumerator '(nil (()) (((1)) 2)(3 4) nil () () (5 7))))
(defparameter *e2* (make-list-enumerator '(4 5 6) t))

(defparameter *e3* (make-mod-inductive-enumerator 0 #'1+ (lambda (x) (mod x 2))))
(defparameter *e4* (make-mod-inductive-enumerator 0 #'1+))

(defparameter *e6* (make-filter-enumerator *e2* #'evenp))
(defparameter *e7* (make-filter-enumerator *e3* #'evenp))

(defparameter *e5* (make-parallel-enumerator *e3* *e2* *e1*))
(defparameter *e8* (make-parallel-enumerator *e1* *e2* *e7*))

(defparameter *e9* (make-append-enumerator *e0*))
(defparameter *e10* (make-append-enumerator *e1*))

(defparameter *e11* (make-memo-enumerator *e0*))
(defparameter *e12* (make-memo-enumerator *e2*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projet de Lisp : Jordan Aupetit - Fabien Berarde - Mickaël Lemasson ;;
;;           Groupe A1 - Licence 3 Informatique - 2012/2013	       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;