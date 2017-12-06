;;; Copyright (c) 2017 William R. Felts III, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; cam.lisp

;;;
;;; "Content Addressable Memory"
;;;
;;; (make-cam :default-test #'equalp :specialized-tests (list #'eq #'eql #'equal #'equalp) :default-value nil)
;;; (camref cam index1 index2 ...)
;;; (setf (camref cam index1 index2 ...) value)
;;; (walk-cam cam predicate)
;;;  predicate => #'(lambda (indexes value))
;;;
;;; Sould I use MAARS (Multiple Associatve ARrayS) instead of CAM?
;;; 

(in-package #:cam)

;;; "cam" goes here. Hacks and glory await!

(defclass cam ()
  ((primary-hash :accessor primary-hash :initarg :primary-hash :initform nil)
   (default-value :accessor default-value :initarg :default-value :initform nil)
   (default-test :accessor default-test :initarg :default-test :initform #'equal)
   (specialized-tests :accessor specialized-tests :initarg :specialized-tests :initform nil)))

(defun valid-test-p (test)
  (member test (list #'eq #'eql #'equal #'equalp)))

;;;
;;; create a CAM object
;;;
(defun make-cam (&key (default-value nil) (default-test #'equal) (specialized-tests nil))
  (unless (every #'valid-test-p specialized-tests)
    (error "a specialized-test is not '#eq '#eql' '#equal or '#equalp"))
  (unless (or (null default-test) (valid-test-p default-test))
    (error "default-test is not '#eq '#eql' '#equal or '#equalp"))
  (unless (or default-test specialized-tests)
    (error "no tests"))
  (let ((main-test (or default-test (first specialized-tests))))
    (make-instance 'cam
		   :primary-hash (make-hash-table :test main-test)
		   :default-value default-value
		   :default-test default-test
		   :specialized-tests specialized-tests)))

;;;
;;; set a CAM object's value
;;;
(defun (setf camref) (value cam &rest indexes)
  (labels
      ((cam-ref-helper (indexes specialized-tests hash-table)
	 (let ((index (first indexes))
	       (test (or (first specialized-tests) (default-test cam))))
	   (multiple-value-bind (next-hash-table foundp)
	       (gethash index hash-table)
	     (declare (ignore next-hash-table))
	     (unless foundp
	       (setf (gethash index hash-table) (make-hash-table :test test)))
	     (if (rest indexes)
		 (cam-ref-helper (rest indexes) (rest specialized-tests) (gethash index hash-table))
		 (setf (gethash index hash-table) value))))))
    (cam-ref-helper indexes (specialized-tests cam) (primary-hash cam))))

;;;
;;; get a CAM object's value
;;;
(defun camref (cam &rest indexes)
  (labels
      ((cam-ref-helper (indexes hash-table)
	 (let ((index (first indexes)))
	   (multiple-value-bind (value foundp)
	       (gethash index hash-table)
	     (if foundp
	       (if (rest indexes)
		   (cam-ref-helper (rest indexes) value)
		   value)
	       (default-value cam))))))
       (cam-ref-helper indexes (primary-hash cam))))

;;;
;;; predicate => #'(lambda (indexes value)
;;;                    ...)
;;;
(defun walk-cam (cam predicate)
  (labels
      ((cam-walk-helper (indexes value)
	 (if (hash-table-p value)
     	     (maphash #'(lambda (key value)
			  (cam-walk-helper (append indexes (list key)) value))
		      value)
	     (funcall predicate indexes value))))
    (maphash #'(lambda (index value)
		 (cam-walk-helper (list index) value))
	     (primary-hash cam))))

;;;
;;; return the primary hash table for custom walkers
;;;
(defun cam-hash (cam)
  (primary-hash cam))

(defun test ()
  (let ((cam (make-cam :default-test #'equalp))
	(result nil)
	(profile (make-cam :default-test #'equalp :default-value 0)))
    (incf (camref profile "xyz.c" 127))
    (incf (camref profile "xyz.c" 127))
    (incf (camref profile "abcd.lisp" 31))
    (incf (camref profile "abcd.lisp" 217))
    (walk-cam profile
		    #'(lambda (indexes value)
			(format t "~{~s~^ ~}:~d~%" indexes value)))
    (setf (camref cam "foo" "bar") 1)
    (setf (camref cam "foo" "baz") 2)
    (setf (camref cam 3 "this") "that")
    (format t "cam[\"foo\" \"bar\"]=~s~%" (camref cam "foo" "bar"))
    (format t "cam[\"foo\" \"baz\"]=~s~%" (camref cam "foo" "baz"))
    (format t "cam[3 \"this\"]=~s~%" (camref cam 3 "this"))
    (format t "cam[3 \"that\"]=~s~%" (camref cam 3 "that"))
    (format t "cam[nil nil]=~s~%" (camref cam nil nil))
    (format t "(camref cam 3)=~s~%" (camref cam 3))
    (format t "(camref cam nil)=~s~%" (camref cam nil))
    (format t "(camref cam 'foo)=~s~%" (camref cam 'foo))
    (walk-cam cam #'(lambda (indexes value)
			    (push (append indexes (list value)) result)))
    result))
  
