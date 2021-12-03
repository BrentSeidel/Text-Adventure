;;;;
;;;; This file contains macros to be used by the adventure game.
;;;;

;;;
;;; This macro defines a set of three functions for dealing with flags.  The
;;; functions set, clear, and test a flag.  The macro requires three parameters:
;;; The name of the group of the flags, the name of the individual flag, and
;;; the bit.  The flags are contained in an integer.  The functions created
;;; take one parameter.  The parameter is a p-list with a :flags entry for the
;;; flags.
;;;
(defMacro def-flag (type name bit)
    `(progn
;       (format t "Defining flag for group ~s, flag ~s, and bit ~s~%" ',type ',name ,bit)
       ;;
       ;; Define function to test a flag
       ;;
       (defun ,(intern (string-upcase (concatenate 'string (symbol-name type) "-is-" (symbol-name name)))) (object)
           (boole boole-and ,bit (getf object :flags)))
       ;;
       ;; Define function to set a flag
       ;;
         (defun ,(intern (string-upcase (concatenate 'string (symbol-name type) "-set-" (symbol-name name)))) (object)
            (setf (getf object :flags) (boole boole-ior ,bit (getf object :flags))))
       ;;
       ;; Define function to clear a flag
       ;;
         (defun ,(intern (string-upcase (concatenate 'string (symbol-name type) "-clr-" (symbol-name name)))) (object)
            (setf (getf object :flags) (boole boole-andc1 ,bit (getf object :flags))))))
;;;
;;; This macro defines a series of flags.  It can be used to define all of the flags in a group.
;;; It is passed two parameters.  The first is the name of the flag group.  The rest is a list
;;; of flag names and bits.  The (def-flag) macro is called for each flag name and bit,
;;;
;;;
;;; Unfortunately, this macro does not work.
;;;
;(defMacro def-flags (type &body body)
;   `(dolist (flag ',body)
;       (let ((name (first flag))
;             (bit (second flag)))
;         (format t "defining functions for type ~a, name ~a, and bit ~a~%" ',type name bit)
;         (defun ,(intern (string-upcase (concatenate 'string (symbol-name type) "-is-" (symbol-name name)))) (object)
;              (boole boole-and bit (getf object :flags)))
;           (defun ,(intern (string-upcase (concatenate 'string (symbol-name type) "-set-" (symbol-name name)))) (object)
;              (setf (getf object :flags) (boole boole-ior bit (getf object :flags))))
;           (defun ,(intern (string-upcase (concatenate 'string (symbol-name type) "-clr-" (symbol-name name)))) (object)
;              (setf (getf object :flags) (boole boole-andc1 bit (getf object :flags)))))))
;         (def-flag ,type ,name bit))))
;;;
;;; This macro generates a test for a condition that may not be present.  If the value is null,
;;; a true is returned.  If the value is not null, it is compared with another value.
;;;
(defMacro equal-null (test value)
   `(or (null ,test) (equal ,test ,value)))
;