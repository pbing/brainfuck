;;; Brainfuck to LLVM compiler
;;; $Id: brainfuck.lisp,v 1.2 2009/12/23 18:10:36 bernd Exp bernd $

(defpackage "BRAINFUCK"
  (:nicknames "BF")
  (:use "COMMON-LISP")
  (:export :compile-file)
  (:shadow :compile-file))

(in-package "BRAINFUCK")

(defvar *head* 0)
(defvar *tape* 0)
(defvar *label* 0)
(defvar *test* 0)
(defvar *loop-stack* '())

(defun llvm-header (stream)
  (let ((head *head*)
	(label *label*))
    (format stream "~&declare void @llvm.memset.i32(i8* nocapture, i8, i32, i32) nounwind~%~
                    declare i32 @getchar()~%~
                    declare i32 @putchar(i32)~2%~
                    define void @main() {~%~
                    main.~D:~%~
                    ~4T%arr = malloc i8, i32 65536~%~
                    ~4Tcall void @llvm.memset.i32(i8* %arr, i8 0, i32 65536, i32 1)~%~
                    ~4T%head.~D = getelementptr i8* %arr, i32 32768~2%" label head)))

(defun llvm-footer (stream)
  (format stream "~&~4Tfree i8* %arr~%~
                  ~4Tret void~%~
                  }~%"))

(defun plus (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = load i8* %head.~D ; +~%" tape0 head)
    (format stream "~4T%tape.~D = add i8 %tape.~D, 1~%" tape1 tape0)
    (format stream "~4Tstore i8 %tape.~D, i8* %head.~D~%" tape1 head)))

(defun minus (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = load i8* %head.~D ; -~%" tape0 head)
    (format stream "~4T%tape.~D = sub i8 %tape.~D, 1~%" tape1 tape0)
    (format stream "~4Tstore i8 %tape.~D, i8* %head.~D~%" tape1 head)))

(defun left (stream)
  (let* ((head0 *head*)
	 (head1 (incf *head*)))
    (format stream "~&~4T%head.~D = getelementptr i8* %head.~D, i32 -1 ; <~%" head1 head0)))

(defun right (stream)
  (let* ((head0 *head*)
	 (head1 (incf *head*)))
    (format stream "~&~4T%head.~D = getelementptr i8* %head.~D, i32 1 ; >~%" head1 head0)))

(defun dot (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = load i8* %head.~D ; .~%" tape0 head)
    (format stream "~4T%tape.~D = sext i8 %tape.~D to i32~%" tape1 tape0)
    (format stream "~4Tcall i32 @putchar(i32 %tape.~D)~%" tape1)))

(defun comma (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = call i32 @getchar() ; , ~%" tape0)
    (format stream "~4T%tape.~D = trunc i32 %tape.~D to i8~%" tape1 tape0)
    (format stream "~4Tstore i8 %tape.~D, i8* %head.~D~%" tape1 head)))

(defun left-bracket (stream)
  (push *head* *loop-stack*)
  (push *label* *loop-stack*)
  (let* ((loop-test (incf *label*))
	 (loop-body (incf *label*)))
    (format stream "~&~4Tbr label %main.~D ; [~2%" loop-test)
    (format stream "main.~D:~%" loop-body)
    (push loop-test *loop-stack*)
    (push loop-body *loop-stack*)))

(defun right-bracket (stream)
  (let* ((label *label*)
	 (loop-body (pop *loop-stack*))
	 (loop-test (pop *loop-stack*))
	 (loop-before (pop *loop-stack*))
	 (head0 (pop *loop-stack*))
	 (head1 *head*)
	 (head2 (incf *head*))
	 (loop-after (incf *label*))
	 (test (incf *test*))
	 (tape (incf *tape*)))
    (format stream "~&~4Tbr label %main.~D ; ]~2%" loop-test)
    (format stream "~&main.~D:~%" loop-test)
    (format stream "~4T%head.~D = phi i8* [%head.~D, %main.~D], [%head.~D, %main.~D]~%"
	    head2 head0 loop-before head1 label)
    (format stream "~4T%tape.~D = load i8* %head.~D~%" tape head2)
    (format stream "~4T%test.~D = icmp eq i8 %tape.~D, 0~%" test tape)
    (format stream "~4Tbr i1 %test.~D, label %main.~D, label %main.~D~2%" test loop-after loop-body)
    (format stream "main.~D:~%" loop-after)))

(defun initialize ()
  (setf *head* 0  *tape* 0
	*label* 0  *test* 0
	*loop-stack* '()))

(defun %compile (string &optional (stream t))
  (loop for c across string do
       (cond ((char= #\+ c) (plus stream))
	     ((char= #\- c) (minus stream))
	     ((char= #\< c) (left stream))
	     ((char= #\> c) (right stream))
	     ((char= #\. c) (dot stream))
	     ((char= #\, c) (comma stream))
	     ((char= #\[ c) (left-bracket stream))
	     ((char= #\] c) (right-bracket stream)))))

(defun compile-file (input-file &key output-file)
  (initialize)
  (let ((outfile (or output-file 
		     (make-pathname :name (pathname-name input-file) :type "ll"))))
    (with-open-file (istream input-file)
      (with-open-file (ostream outfile :direction :output :if-exists :supersede)
	(llvm-header ostream)
	(loop for line = (read-line istream nil nil)
	   while line do (%compile line ostream))
	(llvm-footer ostream)))))
