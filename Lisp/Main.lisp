;;;;
;;;;  This file contains the main loop of the adventure game program
;;;;  and some supporting functions.
;;;;
;;;
;;; include some generic functions
;;;
(defConstant +path+ "/Users/brent/development/lisp/")
(load (concatenate 'string +path+ "Strings.lisp"))
(load (concatenate 'string +path+ "Utility.lisp"))
;;;
;;; Define the package for the game
;;;
(defPackage :bbs.game.adventure
   (:use :bbs.strings :bbs.utils :common-lisp :common-lisp-user)
   (:export :adventure-main))
(in-package :bbs.game.adventure)
;;;
;;; Includes for other parts of the program
;;;
(load (concatenate 'string cl-user::+path+ "/Adventure Game/Macros.lisp"))
;;;
;;; Globally used constants.
;;;
(defConstant +denizen+ "Denizen")  ; Code used to indicate that the player is holding the object
;;;
;;; Some global variables.  These include the program databases and some other state information.
;;; The global variables comprise the state of the system.
;;;
;;
;; Main program databases.  These include the vocabulary table, a message list, rooms, and objects
;; lists.
;;
(defVar *vocabulary* (make-hash-table :test 'equalp)) ; Generally read-only
(defVar *messages* (make-hash-table :test 'equalp))   ; Generally read-only
(defVar *rooms* (make-hash-table :test 'equalp))      ; Updated during the course of the game
(defVar *objects* (make-hash-table :test 'equalp))    ; updated during the course of the game

;;
;; A definition for the adventurer who wanders around the dungeon.  In the future, additional
;; denizens may be added.
;;
(defVar *denizen*)
;;
;; The command prompt for the program.
;;
(defVar *prompt* "~&> ")
;;;
;;; Main program functions
;;;
;;
;; The main function
;;
(defun adventure-main ()
   "The main program entry point."
   (load-data "/Users/brent/Development/lisp/Adventure Game/adventure.data")
   (format t "~&Hello ~A~%" (getf *denizen* :name))
   (format t (gethash "start" *messages*))
   (list-objects-at (current-location))
   (let ((cmd) (word) (rest) (func) (vocab))
        (loop
          (setq cmd (split-words (string-upcase (ask *prompt*))))
          (setq word (car cmd))
          (setq rest (cdr cmd))
          (setq vocab (gethash word *vocabulary*))
          (setq func (getf vocab :funct))
          (when (getf vocab :quit) (return))
          (if func
              (funcall func word rest)
              (format t "~&The command ~S was not understood.~%" cmd))
            (finish-output *standard-output*))))
;;;
;;; Utility functions.  These perform various basic things on the program state
;;;
;;;
;;; Parsing functions
;;;
;;
;; Extract the next object from the command string.  Returns a list containing
;; two items.  The first is the object and the second is the remaining command
;; string.
;;
(defun extract-object (cmd)
  "Extract the next object from the command string.  Returns a list containing
two items.  The first is the object and the second is the remaining command
string."
  (let ((adj ())
        (noun)
        (word)
        (props)
        (rest cmd))
    (loop
      (setq word (car rest))
      (setq rest (cdr rest))
      (setq props (gethash word *vocabulary*))
      (format t "~&Processing word <~A> with properties <~A>~%" word (gethash word *vocabulary*))
      (cond
       ((getf props :noun) ; Is the current word a noun?
        (format t "~&Found noun <~A>~%" word)
        (setq noun word)
        (return (list (list adj noun) rest)))
       ((getf props :adj)  ; Is the current word an adjective?
        (format t "~&Found adjective <~A>~%" word)
        (setf adj (cons (intern word) adj))
        (format t "~&Adjective list set to <~A>~%" adj))
       ((getf props :article) ; Ignore articles (a, the)
        (format t "~&Ignoring article <~A>~%" word))
       ((null rest)
        (format t "~&End of list reached.~%")
          (return nil))
       (t (format t "~&Word <~A> is unknown~%" word))))))
;;;
;;; Functions dealing with objects.
;;;
;;
;; Generate a function to match an object
;;
(defun match-object (&key name adj loc)
   "Generates a function that matches on the name, adjective, and location
of an object.  All items are optional."
   #'(lambda (obj)
;(format t "~&Is <~A> a subset of <~A> result <~A>~%" adj (getf obj :adjs) (subsetp adj (getf obj :adjs)))
       (and
        (equal-null name (string-upcase (getf obj :name)))
        (or (null adj) (subsetp adj (getf obj :adjs)))
        (equal-null loc (getf obj :location)))))
;;
;; Return a list of objects that match the criteria function
;;
(defun find-objects (criteria)
   "Does a select on the object list.  It returns a list of all the
object keys where the criteria function returns a true value."
   (let ((list ())
         (more?)
         (key)
         (obj))
     (with-hash-table-iterator (next-entry *objects*)
        (loop
          (multiple-value-bind (more? key obj) (next-entry)
             (unless more? (return))
             (if (funcall criteria obj)
               (push key list)
               nil))))
     list))
;;
;;
;;  Prints a list of objects at the current location
;;
(defun list-objects-at (loc)
   (let ((keys (find-objects (match-object :loc loc)))
         (obj))
     (dolist (key keys)
        (setf obj (gethash key *objects*))
        (format t (getf obj :long)))))
;;
;; Picks up an object - sets the object location to be the player.
;;
(defun pick-up-object (obj)
   (setf (getf obj :location) +denizen+))
;;
;; Deposits an object - sets its location to the current location.
;;
(defun deposit-object (obj)
   (setf (getf obj :location) (current-location)))
;;;
;;; Functions dealing with locations.
;;;
;;
;; Get the current location
;;
(defun current-location ()
   "Return the current location of the adventurer."
   (getf *denizen* :loc))
;;
;; Set the current location to a specific value
;;
(defun set-current-location (loc)
   "Sets the current location to the specified location.  Checks to make sure that
the room actually exists and then after setting the location, sets the room entered
flag"
   (let ((room (gethash loc *rooms*)))
         (if room
           (progn
             (setf (getf *denizen* :loc) loc)
             (room-set-entered room))
           (format t "Internal error - can't set to room ~S~%" loc))))
;;
;; Print a description of the current location.  It checks if the room has
;; been entered and if it is light.
;;
(defun describe-current-location ()
   "Prints a description of the current location.  It includes tests
to see if this location has been visited before and if it is dark."
   (let ((loc (current-location)))
     (if (room-is-light (gethash loc *rooms*))
       (progn
          (if (room-is-entered (gethash loc *rooms*))
            (format t (getf (gethash loc *rooms*) :short))
            (format t (getf (gethash loc *rooms*) :long)))
          (list-objects-at (current-location)))
       (format t (gethash "dark" *messages*)))))
;;
;; Find the exit for the specified direction from the specified room
;;
(defun find-exit (dir room)
        (let ((exits (getf room :exits)) (test))
             (loop
               (setq test (car exits))
               (setq exits (cdr exits))
               (when (equalp dir (getf test :dir)) (return test))
               (when (equal nil exits) (return nil)))))
;;;
;;; Various flags for things
;;;
;;
;; Object flags
;;
(def-flag obj immobile #x00000001)
(def-flag obj key      #x00000002)
(def-flag obj weapon   #x00000004)
(def-flag obj food     #x00000008)
(def-flag obj drink    #x00000010)
(def-flag obj switch   #x00000020)
(def-flag obj poison   #x00000040)
(def-flag obj read     #x00000080)
(def-flag obj on       #x00000100)
(def-flag obj denizen  #x00000200)
(def-flag obj vehicle  #x00000400)
(def-flag obj door     #x00000800)
(def-flag obj open     #x00001000)
(def-flag obj locked   #x00002000)
(def-flag obj openable #x00004000)
(def-flag obj lockable #x00008000)
(def-flag obj special  #x00010000)
(def-flag obj light    #x00020000)
;;
;; Room flags
;;
(def-flag room entered #x00000001)
;sub	is_entered1		{$_[0]->{"_flags"} & oct("0x00000002")}
;sub	is_entered2		{$_[0]->{"_flags"} & oct("0x00000004")}
;sub	is_entered3		{$_[0]->{"_flags"} & oct("0x00000008")}
;sub	is_entered4		{$_[0]->{"_flags"} & oct("0x00000010")}
;sub	is_entered5		{$_[0]->{"_flags"} & oct("0x00000020")}
;sub	is_entered6		{$_[0]->{"_flags"} & oct("0x00000040")}
;sub	is_entered7		{$_[0]->{"_flags"} & oct("0x00000080")}
(def-flag room light    #x00000100)
(def-flag room air      #x00000200)
(def-flag room prints   #x00000400)
(def-flag room ship     #x00000800)
;;
;; Exit flags
;;
(def-flag exit closed #x0001)
(def-flag exit start  #x0002)
;sub	is_flag02		{$_[0]->{"_flags"} & oct("0x0004")}
;sub	is_flag03		{$_[0]->{"_flags"} & oct("0x0008")}
;sub	is_flag04		{$_[0]->{"_flags"} & oct("0x0010")}
;sub	is_flag05		{$_[0]->{"_flags"} & oct("0x0020")}
;sub	is_flag06		{$_[0]->{"_flags"} & oct("0x0040")}
;sub	is_flag07		{$_[0]->{"_flags"} & oct("0x0080")}
;sub	is_flag08		{$_[0]->{"_flags"} & oct("0x0100")}
;sub	is_flag09		{$_[0]->{"_flags"} & oct("0x0200")}
;sub	is_flag10		{$_[0]->{"_flags"} & oct("0x0400")}
;sub	is_flag11		{$_[0]->{"_flags"} & oct("0x0800")}
;sub	is_flag12		{$_[0]->{"_flags"} & oct("0x1000")}
;sub	is_flag13		{$_[0]->{"_flags"} & oct("0x2000")}
;sub	is_flag14		{$_[0]->{"_flags"} & oct("0x4000")}
;sub	is_flag15		{$_[0]->{"_flags"} & oct("0x8000")}

;;;
;;; Data loading functions.  These read from the configuration file and build
;;; the datastructures and databases needed for the program.
;;;
;;
;; Function to load data from a file.
;;
(defun load-data (file-name)
   "This function reads data from a configuration file and builds the game databases."
   (let ((data) (cmd) (rest))
        (with-open-file (config file-name :direction :input)
           (loop
             (setq data (read config))
             (setq cmd (car data))
             (setq rest (cdr data))
             (when (eql cmd :done) (return))
             (cond
              ((eq cmd :denizen) (set-denizen rest))
              ((eq cmd :obj) (add-object rest))
              ((eq cmd :room) (add-room rest))
              ((eq cmd :msg) (add-message rest))
              ((eq cmd :vocab) (add-vocab rest))
              ((eq cmd :print) (format t (car rest)))
              (t (format t "~&Unrecognized input object ~S~%" data)))))))
;;;
;;; Functions to load the various kinds of data.  The main difference is which table
;;; is loaded.
;;;
(defun add-message (data)
   (setf (gethash (car data) *messages*) (second data)))
;;
;; Adding an object is a little more complex since the object's name is added to the
;; vocabulary list as a noun.  At some point, objects will also have adjectives
;; associated with them that will also need to be added to the vocabulary.
;;
(defun add-object (data)
  (let ((key (string-upcase (car data)))
        (value (cdr data)))
    (let ((name (getf value :name))
          (adjs (getf value :adjs)))
      (setf (getf (gethash name *vocabulary*) :noun) 1)
      (mapcar (lambda (x) (setf (getf (gethash x *vocabulary*) :adj) 1)) adjs)
      (setf (gethash key *objects*) value))))
;;
(defun add-room (data)
   (setf (gethash (car data) *rooms*) (cdr data)))
;;
(defun set-denizen (data)
   (setf *denizen* data))
;;
(defun add-vocab (data)
   (setf (gethash (car data) *vocabulary*) (cdr data)))
;;;===========================================================================
;;;
;;; Main program command functions
;;;
;;
;; Move in a specific direction.  First a function to determine which direction to go.
;;
(defun determine-direction (cmd args)
   (let ((dir (getf (gethash cmd *vocabulary*) :dir)))
        (if dir
            dir
            (if args
                (progn (let ((words (first-word args)))
                             (determine-direction (car words) (cadr words))))
                nil))))
;;
;; Then a function to go in that direction
;;
(defun move-> (dir)
   (let ((exit (find-exit dir (gethash (current-location) *rooms*))))
;     (format t "Moving denizen ~S in direction ~S using exit ~S~%" *denizen* dir exit)
     (if (eql nil exit)
       (format t "There is an error in the database such that there is no exit entry for the direction ~A for the current location~%" dir)
       (let ((dest (getf exit :dest)))
         (if dest
           (set-current-location dest)
           (format t (gethash (getf exit :closed) *messages*))))))
   (describe-current-location))

(defun goto-location (cmd args)
   (let ((dir (determine-direction cmd args)))
        (if dir
            (move-> dir)
            (format t (gethash "can't" *messages*) cmd args))))
;;
;; Describe an object or a room
;;
(defun room-look (cmd args)
   (declare (ignore cmd))
   (declare (ignore args))
   (describe-current-location))

(defun describe-data (cmd args)
  (declare (ignore cmd))
  (declare (ignore args))
  (format t "~&The command table is:~%")
  (prin1 *vocabulary* *standard-output*)
  (format t "~%"))
;;
;; Print a help message
;;
(defun print-help (cmd args)
   (declare (ignore cmd))
   (declare (ignore args))
   (format t (gethash "help" *messages*)))
;;
;; Quit the program
;;
(defun quit-program (cmd args)
   (declare (ignore cmd))
   (declare (ignore args))
   (format t "~&Good bye!~%")
   (common-lisp-user::quit))
;;;-------------------------------------------
;;; Debugging commands
;;;
(defun dump-hash (hash)
  (foreach (key (hash-keys hash))
           (format t "~&~%For key <~A> data is:~%  " key)
           (prin1 (gethash key hash))))
;;
;; Dispatch Debugging
;;
(defun debug-program (cmd args)
  (prin1 cmd)
  (prin1 args)
  (let ((t1 (car args))
        (t2 (second args)))
    (cond
     ((equalp t1 "BREAK") (break "Debugging program ~A ~A ~%" cmd args))
     ((equalp t1 "DUMP") (cond
                          ((equalp t2 "VOCAB") (dump-hash *vocabulary*))
                          ((equalp t2 "ROOMS") (dump-hash *rooms*))
                          ((equalp t2 "MESSAGE") (dump-hash *messages*))
                          ((equalp t2 "OBJECTS") (dump-hash *objects*))
                          (T (format t "~&Unknown data structure ~A~%" t2))))
     (T (format t "~&Unknown debug command ~A~%" t1)))))
;;
;; Describe the current room
;;
(defun describe-room (cmd args)
  (declare (ignore cmd))
  (declare (ignore args))
  (format t "~&Current room is <~A>~%" (current-location))
  (format t "~&~A~%" (gethash (current-location) *rooms*))
)
;;
;; Go to specified location
(defun teleport (cmd args)
  (declare (ignore cmd))
  (set-current-location args)
)
;;;------------------------------------------------
;;; Commands to manupulate objects
;;
;; Get an object
;;
(defun get-object (cmd args)
  (declare (ignore cmd))
  (let ((parsed (extract-object args))
        (object)
        (adjs)
        (noun)
;        (rest)
        (id))
    (setq object (car parsed))
;    (setq rest (cdr parsed))
    (setq adjs (car object))
    (setq noun (cadr object))
    (setq id (find-objects (match-object :name noun :adj adjs :loc (current-location))))
    (format t "~&Getting object <~A>, found object ids <~A>~%" object id)
    (pick-up-object (gethash (car id) *objects*))))
;;
;; Drop an object
;;
(defun drop-object (cmd args)
  (declare (ignore cmd))
  (let ((parsed (extract-object args))
        (object)
        (adjs)
        (noun)
;        (rest)
        (id))
    (setq object (car parsed))
;    (setq rest (cdr parsed))
    (setq adjs (car object))
    (setq noun (cadr object))
    (setq id (find-objects (match-object :name noun :adj adjs :loc +denizen+)))
    (format t "~&Droppinging object <~A>, found object ids <~A>~%" object id)
    (deposit-object (gethash (car id) *objects*))))
;;
;; List the current inventory of objects
;;
(defun list-inventory (cmd args)
  (declare (ignore cmd))
  (declare (ignore args))
  (list-objects-at +denizen+))
;;;------------------------------------------------
;;;
;;; Run the main program
;;;
(adventure-main)
(in-package :common-lisp-user)
