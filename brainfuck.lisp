;;; Brainfuck to LLVM compiler
;;; $Id: brainfuck.lisp,v 1.4 2009/12/24 16:57:59 bernd Exp $

(defpackage "BRAINFUCK"
  (:nicknames "BF")
  (:use "COMMON-LISP")
  (:export :compile-file)
  (:shadow :compile-file))

(in-package "BRAINFUCK")

(defvar *head* 0)
(defvar *tape* 0)
(defvar *label* 0)
(defvar *last-label* 0)
(defvar *test* 0)
(defvar *loop-stack* '())

(defun llvm-header (stream)
  (let ((head *head*)
	(label *label*))
    (format stream "~&declare void @llvm.memset.i64(i8* nocapture, i8, i64, i1) nounwind~%~
                    declare i64 @getchar()~%~
                    declare i64 @putchar(i64)~2%~
                    declare i8* @malloc(i64)~2%~
                    declare void @free(i8*)~2%~
                    define void @main() {~%~
                    main.~D:~%~
                    ~4T%arr = call i8* @malloc(i64 65536)~%~
                    ~4Tcall void @llvm.memset.i64(i8* %arr, i8 0, i64 65536, i1 1)~%~
                    ~4T%head.~D = getelementptr i8, i8* %arr, i64 32768~2%" label head)))

(defun llvm-footer (stream)
  (format stream "~&~4T call void @free(i8* %arr)~%~
                  ~4Tret void~%~
                  }~%"))

(defun plus (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = load i8, i8* %head.~D ; +~%" tape0 head)
    (format stream "~4T%tape.~D = add i8 %tape.~D, 1~%" tape1 tape0)
    (format stream "~4Tstore i8 %tape.~D, i8* %head.~D~%" tape1 head)))

(defun minus (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = load i8, i8* %head.~D ; -~%" tape0 head)
    (format stream "~4T%tape.~D = sub i8 %tape.~D, 1~%" tape1 tape0)
    (format stream "~4Tstore i8 %tape.~D, i8* %head.~D~%" tape1 head)))

(defun left (stream)
  (let* ((head0 *head*)
	 (head1 (incf *head*)))
    (format stream "~&~4T%head.~D = getelementptr i8, i8* %head.~D, i64 -1 ; <~%" head1 head0)))

(defun right (stream)
  (let* ((head0 *head*)
	 (head1 (incf *head*)))
    (format stream "~&~4T%head.~D = getelementptr i8, i8* %head.~D, i64 1 ; >~%" head1 head0)))

(defun dot (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = load i8, i8* %head.~D ; .~%" tape0 head)
    (format stream "~4T%tape.~D = sext i8 %tape.~D to i64~%" tape1 tape0)
    (format stream "~4Tcall i64 @putchar(i64 %tape.~D)~%" tape1)))

(defun comma (stream)
  (let* ((head *head*)
	 (tape0 (incf *tape*))
	 (tape1 (incf *tape*)))
    (format stream "~&~4T%tape.~D = call i64 @getchar() ; , ~%" tape0)
    (format stream "~4T%tape.~D = trunc i64 %tape.~D to i8~%" tape1 tape0)
    (format stream "~4Tstore i8 %tape.~D, i8* %head.~D~%" tape1 head)))

(defun left-bracket (stream)
  (let* ((head0 *head*)
	 (head1 (incf *head*))
	 (loop-before *last-label*)
	 (loop-test (incf *label*))
	 (loop-body (incf *label*))	 
	 (loop-after (incf *label*)))
    (format stream "~&~4Tbr label %main.~D ; [~2%" loop-test)
    (format stream "main.~D: ; loop-body~%" loop-body)
    (push head0 *loop-stack*)
    (push head1 *loop-stack*)
    (push loop-before *loop-stack*)
    (push loop-test *loop-stack*)
    (push loop-body *loop-stack*)
    (push loop-after *loop-stack*)
    (setf *last-label* loop-body)))

(defun right-bracket (stream)
  (let* ((loop-after (pop *loop-stack*))
	 (loop-body (pop *loop-stack*))
	 (loop-test (pop *loop-stack*))
	 (loop-before (pop *loop-stack*))
	 (head2 (pop *loop-stack*))
	 (head0 (pop *loop-stack*))
	 (head1 *head*)
	 (head3 (incf *head*))
	 (last-label *last-label*)
	 (tape (incf *tape*))
	 (test (incf *test*)))
    (format stream "~&~4Tbr label %main.~D ; ]~2%" loop-test)
    (format stream "main.~D: ; loop-test~%" loop-test)
    (format stream "~4T%head.~D = phi i8* [%head.~D, %main.~D], [%head.~D, %main.~D]~%"
	    head2 head0 loop-before head1 last-label)
    (format stream "~4T%tape.~D = load i8, i8* %head.~D~%" tape head2)
    (format stream "~4T%test.~D = icmp eq i8 %tape.~D, 0~%" test tape)
    (format stream "~4Tbr i1 %test.~D, label %main.~D, label %main.~D~2%" test loop-after loop-body)
    (format stream "main.~D: ; loop-after~%" loop-after)
    (format stream "~4T%head.~D = phi i8* [%head.~D, %main.~D]~%" head3 head2 loop-test)
    (setf *last-label* loop-after)))

(defun initialize ()
  (setf *head* 0  *tape* 0
	*label* 0  *last-label* 0  *test* 0
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
