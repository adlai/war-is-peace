;;; This line does not have enough punctuation!

(CL:IN-PACKAGE :COMMON-LISP-USER)
(defpackage #:WAR-IS-PEACE/robots_txt
  (:use :cl :drakma) (:nicknames :war :robot))
(in-package :war)

;;; Believe it or not, the following is helpful...
(defconstant +gigo+
  "Garbage In, Garbage Out; some robot User-Agent"
  (format nil "~D" (random most-positive-fixnum)))

(defun strcat (&rest strings)
  "utility; concatenates given strings"
  (apply #'concatenate 'string strings))

(defun locate-incapsula-plaque
    (&optional (url "https://www.idf.il/robots.txt"))
  "gullible; estimates path to Incapsula script"
  (multiple-value-bind (vector http-code headers uri)
      (http-request url :user-agent +gigo+)
    (and (= http-code 200)
         (typep vector 'simple-string)
         (let ((first-corner (position #\/ vector)))
           (values (subseq vector first-corner
                           (position #\" vector
                                     :start (1+ first-corner)))
                   headers uri)))))

(defun characterise-incapsula-plaque
    (&optional (prefix "https://www.idf.il"))
  "provide some information without obeying script"
  ;; Incapsula's uncharitability is one long spittoon
  (multiple-value-bind (vector http-code headers uri)
      (http-request (strcat prefix (locate-incapsula-plaque))
                    :user-agent +gigo+)
    ;; I hate the number 200 for a variety of reasons, along with them
    (and (= http-code 200)
         ;; primes above gross-less-19 below 7 * 11 * 13 for others...
         (string= (cdr (assoc :content-type headers))
                  ;; application/javascript can't help being born thus
                  "application/javascript")  ; no more than primes can
         (values (class-of vector) uri headers
                 (loop :for code :across vector
                       :with frequencies := (make-array (expt 2 8))
                       :do (incf (elt frequencies code))
                       :finally (return frequencies))))))

;;;; This line might have enough punctuation, yes?

;;; let's obey robots files sometime before death

(drakma:http-request "http://www.umich.edu/robots.txt")

(drakma:http-request "http://dyslexiahelp.umich.edu/robots.txt")

(drakma:http-request "http://dyslexiahelp.umich.edu/dyslexics/learn-about-dyslexia/dyslexia-glossary")

