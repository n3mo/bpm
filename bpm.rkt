#! /usr/bin/env racket
#lang racket

;;; Dependencies
(require charterm)
(require racket/path)

;;; Current version
(define bpm-version "0.0.1 (2017-01-04)")

;;; Parameters ----------------------------------------

;;; Current page of results
(define page (make-parameter 1))
;;; Cursor position
(define cursor (make-parameter 3))
;;; Cursor wrap from beginning<->end of list?
(define cursor-wrap (make-parameter #t))
;;; Row number of start of file listing
(define file-row (make-parameter 3))
;;; Dialog location (top row of dialog box)
(define dialog-row (make-parameter 20))
;;; Current working directory root
(define root-path (make-parameter #f))
;;; Status message? When set to true, the main read key loop will
;;; pause extra long, before clearing the message and redrawing the
;;; screen
(define message? (make-parameter #f))

;;; Files ending in the following extensions will be
;;; included in all operations. Add accordingly
(define valid-extensions
  '(#".flv" #".mov" #".mp4" #".wmv" #".mkv" #".mov" #".avi" #".mpg" #".swf"))

;;; The script expects a user-supplied target path at runtime.
;;; If no path is supplied, the following default is used. For
;;; more general audiences, this should probably be changed to "."
;; (define default-path "~/media/Television/")
;; (define default-path "~/main/work/teach/resources/intro_psych/videos/")
(define default-path "~/Desktop/tmp/")

;;; Location of trash directory on this system. For now, this assumes
;;; that you're either using Mac OS X or linux...
(define trash-path
  (if (equal? 'macosx (system-type))
      (expand-user-path "~/.Trash/")
      (expand-user-path "~/.local/share/Trash/files/")))

;;; Find valid video files in directory (recursively)
(define (file-list [starting-path default-path])
  (find-files (λ (x) (member (path-get-extension x) valid-extensions))
	      (expand-user-path starting-path)))


(define (%charterm:string-pad-or-truncate str width)
  (let ((len (string-length str)))
    (cond ((= len width) str)
          ((< len width) (string-append str (make-string (- width len) #\space)))
          (else (substring str 0 width)))))

(define (%charterm:bytes-pad-or-truncate bstr width)
  (let ((len (bytes-length bstr)))
    (cond ((= len width) bstr)
          ((< len width)
           (let ((new-bstr (make-bytes width 32)))
             (bytes-copy! new-bstr 0 bstr)
             new-bstr))
          (else (subbytes bstr 0 width)))))

(define (draw-box cols rows)
  (let ((width (- cols 2))
	(height (- rows 2)))
    (charterm-cursor 1 1)
    ;; Box corner upper-left
    ;; (charterm-display "\u2554")
    (charterm-display "|")
    ;; Top edge of box
    ;; (charterm-display (make-string width #\u2550))
    (charterm-display (make-string width #\=))
    ;; Box corner upper-right
    ;; (charterm-display "\u2557")
    (charterm-display "|")
    (charterm-newline)
    ;; Left side
    (for-each
     (λ (x)
       (charterm-cursor 1 x)
       ;; (charterm-display "\u2551"))
       (charterm-display "|"))
     (range 2 height))
    ;; Right side
    (for-each
     (λ (x)
       (charterm-cursor (+ width 2) x)
       ;; (charterm-display "\u2551"))
       (charterm-display "|"))
     (range 2 height))

    ;; Box corner lower-left
    (charterm-cursor 1 height)
    ;; (charterm-display "\u255A")
    (charterm-display "|")
    ;; Bottom edge of box
    ;; (charterm-display (make-string width #\u2550))
    (charterm-display (make-string width #\=))
    ;; Box corner upper-right
    ;; (charterm-display "\u255D")))
    (charterm-display "|")))


;;; Use this to determine which video file is listed first on the
;;; current page
;; (define (page-lead lst-length rows)
;;   (define max-vid (- rows 3))
;;   ;;; (define max-vid 9) ;; debug
;;   (let ((tmp (add1 (* (sub1 (page)) max-vid))))
;;     (if (> (+ tmp (sub1 max-vid)) lst-length)
;; 	(max 0 (- lst-length max-vid))
;; 	tmp)))
(define (page-lead lst-length rows)
  (define max-vid (- rows 3))
  ;;; (define max-vid 9) ;; debug
  (let ((tmp (add1 (* (sub1 (page)) max-vid))))
    (if (> (+ tmp (sub1 max-vid)) lst-length)
	(max 0 (- lst-length max-vid))
	(sub1 tmp))))

;;; Displays video titles, along with any meta-data info
(define (display-titles lst cols rows
			#:numbers? (numbers? #f)
			#:clear? (clear? #f)
			#:draw-cursor? (draw-cursor? #t)
			#:pageless? (pageless? #f))

  (define max-vid (- rows 3))
  ;;; (define max-vid 9) ;; debug
  
  (unless pageless?
    ;; Don't let the user scroll past the last page of results
    (when (> (* (sub1 (page)) max-vid) (length lst)) (page (sub1 (page))))
    (when (<= (page) 0) (page 1)))
  (define start-point (page-lead (length lst) rows))

  ;; End point. This is determined by the lesser of max-vid or the
  ;; number of videos in the list
  (define end-point
    (let ((n (length (drop lst start-point))))
      (if (< n max-vid)
	  (+ (sub1 (file-row)) n)
	  (min max-vid n))))

  ;; Correct the selection cursor if necessary
  (unless pageless?
    (if (cursor-wrap)
	;; Wrap cursor when top/bottom is reached
	(begin
	  (when (< (cursor) (file-row)) (cursor end-point))
	  (when (> (cursor) end-point) (cursor (file-row))))

	;; Do NOT wrap cursor when top/bottom is reached
	(begin
	  (when (< (cursor) (file-row)) (cursor (file-row)))
	  (when (> (cursor)
		   (min max-vid (length (drop lst start-point))))
	    (cursor max-vid)))))

  ;; Clear all rows. This is only necessary when displaying a short
  ;; list of files on top of a long list (e.g., when displaying files
  ;; to be deleted).
  ;; Clear screen manually to avoid calling full clear screen
  (when clear?
    (for-each
     (λ (x)
       (charterm-cursor 1 x)
       (charterm-clear-line-right))
     (range 1 rows)))

  (let loop ((pos (file-row)) (videos (drop lst start-point)))
    (charterm-cursor 2 pos)
    (charterm-clear-line-right)
    
    (when numbers?
      (begin
	(charterm-cursor 3 pos)
	(charterm-display (format "~A" (- pos (sub1 (file-row)))))))

    (charterm-cursor 8 pos)
    (if (and draw-cursor? (= pos (cursor)))
	(begin
	  (charterm-inverse)
	  (charterm-display
	   (path->string (file-name-from-path (car videos))))
	  (charterm-normal))
	(charterm-display
	 (path->string (file-name-from-path (car videos)))))
    (unless (or (= pos max-vid) (null? (cdr videos)))
      (loop (+ pos 1) (cdr videos))))
  ;; Redraw UI interface around file list
  (charterm-cursor 3 2)
  (charterm-clear-line-right)
  (charterm-inverse)
  (charterm-display (format "File list (~A files) (page ~A)"
					       (length lst)
					       (page)))
  (charterm-normal)
  (draw-box cols rows))

;;; Call this whenever anything is updated
(define (draw-ui video-files cols rows
		 #:numbers? (numbers? #f)
		 #:delete? (delete? #f)
		 #:draw-cursor? (draw-cursor? #t)
		 #:pageless? (pageless? #f))
  (display-titles video-files cols rows
		  #:numbers? numbers?
		  #:clear? #t
		  #:draw-cursor? draw-cursor?
		  #:pageless? pageless?)

  ;; Delete mode UI -------------------------
  (when delete?    
    (charterm-cursor 3 (- rows 1))
    (charterm-display "Delete file(s)?  [Y]es  ")
    (let ((keyinfo (charterm-read-keyinfo #:timeout #f)))
      (let ((keycode (charterm-keyinfo-keycode keyinfo)))
	    (case keycode
	      ;; Confirm deletion
	      ((#\Y)
	       (trash-files video-files))
	      (else
	       (begin
		 (charterm-cursor 3 (- rows 1))
		 (charterm-clear-line-right)
		 (charterm-display "Deletion Aborted")
		 (message? #t))))))))

;;; Play video file at cursor
(define (play-selection lst rows)
  (let* ((idx (page-lead (length lst) rows))
	 (path (list-ref (drop lst idx) (- (cursor) (file-row))))
	 (output-dump (open-output-string))
	 (error-dump (open-output-string)))
    (parameterize ([current-error-port error-dump]
		   [current-output-port output-dump])
      (thread (λ ()
		(system* (find-executable-path "mplayer")
			 (path->string (expand-user-path path))))))))

;;; Dialog for deleting files
(define (delete-selection lst cols rows)
  (let* ((idx (page-lead (length lst) rows))
	 (path (list-ref (drop lst idx) (- (cursor) (file-row))))
	 (dir (let-values ([(a b c) (split-path path)]) a))
	 (file-name (regexp-replace #px"\\.[[:alnum:]]{3,4}$"
				    (path->string (file-name-from-path path)) ""))
	 (files
	  (find-files
	   (λ (x)
	     (let ([tmp (file-name-from-path x)])
	       (if tmp
		 (string=? file-name
			   (regexp-replace #px"\\.[[:alnum:]]{3,4}$"
					   (path->string tmp) ""))
		 #f)))
	   dir)))
    (draw-ui files cols rows
	     #:delete? #t
	     #:draw-cursor? #f
	     #:pageless? #t)))

;;; Function for actually trashing files
(define (trash-files lst)
  (for-each
   (λ (x)
     (let ((ext (bytes->string/latin-1 (path-get-extension x)))
	   (file-name (regexp-replace #px"\\.[[:alnum:]]{3,4}$"
				      (path->string (file-name-from-path x)) "")))
       (rename-file-or-directory
	x
	(string->path (string-append (path->string trash-path)
				     file-name ext)))))
   lst))

;;; Display file information
(define (file-info lst cols rows)
  (if (> rows 4)
      (let* ((idx (page-lead (length lst) rows))
	     (path (list-ref (drop lst idx) (- (cursor) (file-row))))
	     (box-top (- (round (/ rows 2)) 1))
	     ;; (box-bottom (+ box-top 7))
	     (box-left 4)	     
	     (box-width (- cols (* box-left 2)))
	     (box-right (sub1 (+ box-left box-width))))
	(charterm-cursor box-left box-top)
	(charterm-display (make-string box-width #\+))
	
	(charterm-cursor box-left (+ box-top 1))
	(charterm-display (make-string 1 #\+))
	(charterm-display " " #:width (- box-width 2))
	(charterm-display (make-string 1 #\+))
	
	(charterm-cursor box-left (+ box-top 2))
	(charterm-display (make-string 1 #\+))
	(charterm-display " " #:width (- box-width 2))
	(charterm-cursor (- (round (/ cols 2)) 4) (+ box-top 2))
	(charterm-display "FILE PATH:")
	(charterm-cursor box-right (+ box-top 2))
	(charterm-display (make-string 1 #\+))

	(charterm-cursor box-left (+ box-top 3))
	(charterm-display (make-string 1 #\+))
	(charterm-display "  " (path->string (path->complete-path path)) #:width (- box-width 3))
	(charterm-display " ")
	(charterm-display (make-string 1 #\+))

	(charterm-cursor box-left (+ box-top 4))
	(charterm-display (make-string 1 #\+))
	(charterm-display " " #:width (- box-width 2))
	(charterm-display (make-string 1 #\+))
	
	(charterm-cursor box-left (+ box-top 5))
	(charterm-display (make-string box-width #\+))

	(charterm-cursor cols (add1 rows)))
      ;; Terminal is too small, display warning
      (begin (charterm-cursor 1 1)
	     (charterm-clear-line-right)
	     (charterm-display "Your terminal window is too small"))))

;; (define (%charterm:demo-input-redraw di)
;;   (charterm-cursor (%charterm:demo-input-x di)
;;                    (%charterm:demo-input-y di))
;;   (charterm-normal)
;;   (charterm-underline)
;;   (charterm-display (%charterm:demo-input-bytes di)
;;                     #:width (%charterm:demo-input-width di))
;;   (charterm-normal))

(define (run-bpm)
  (let ((data-row 12)
	(video-files (file-list (root-path))))
    (with-charterm
     (let ((ct (current-charterm)))
       (let/ec done-ec
         (let loop-remember-read-screen-size ((last-read-col-count 0)
                                              (last-read-row-count 0))

           (let loop-maybe-check-screen-size ()
             (let*-values (((read-col-count read-row-count)
                            (if (or (equal? 0 last-read-col-count)
                                    (equal? 0 last-read-row-count)
                                    (not (charterm-byte-ready?)))
                                (charterm-screen-size)
                                (values last-read-col-count
                                        last-read-row-count)))
                           ((read-screen-size? col-count row-count)
                            (if (and read-col-count read-row-count)
                                (values #t
                                        read-col-count
                                        read-row-count)
                                (values #f
                                        (or read-col-count 80)
                                        (or read-row-count 24))))
                           ((read-screen-size-changed?)
                            (not (and (equal? read-col-count
                                              last-read-col-count)
                                      (equal? read-row-count
                                              last-read-row-count)))))
               ;; Did screen size change?
               (if read-screen-size-changed?

                   ;; Screen size changed.
                   (begin
		     ;; Set window parameters
		     (dialog-row (- (round (/ read-row-count 2)) 3))
		     (charterm-clear-screen)
		     (draw-ui video-files read-col-count read-row-count)		     
		     (loop-remember-read-screen-size read-col-count
						     read-row-count))
                   ;; Screen size didn't change (or we didn't check).
                   (begin

                     (let loop-fast-next-key ()
                       ;; (%charterm:demo-input-put-cursor di)
                       (let ((keyinfo (charterm-read-keyinfo #:timeout (if (message?) 2 1))))
			 (if keyinfo
                             (let ((keycode (charterm-keyinfo-keycode keyinfo)))

                               (case keycode				 
				 ((left pgup)
				  (begin (page (sub1 (page)))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((right pgdn)
				  (begin (page (add1 (page)))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((up)
				  (begin (cursor (sub1 (cursor)))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((down)
				  (begin (cursor (add1 (cursor)))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((#\A)
				  (begin (cursor (- (cursor) 4))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((#\B)
				  (begin (cursor (+ 4 (cursor)))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((#\space)
				  (play-selection video-files read-row-count)
				  (loop-fast-next-key))
				 ((#\n)
				  (draw-ui video-files
					   read-col-count
					   read-row-count
					   #:numbers? #t)
				  (loop-fast-next-key))
				 ;; Deleting files
				 ((#\d)
				  (delete-selection video-files
						    read-col-count
						    read-row-count)
				  (loop-fast-next-key))
				 ;; File information
				 ((#\i)
				  (file-info video-files
					     read-col-count
					     read-row-count)
				  (loop-fast-next-key))
				 ;; ((backspace)
				 ;;  (%charterm:demo-input-backspace di)
				 ;;  (loop-fast-next-key))
				 ;; ((delete)
				 ;;  (%charterm:demo-input-delete di)
				 ;;  (loop-fast-next-key))
				 ((#\q)
				  (begin
				    (charterm-clear-screen)
				    (charterm-display "Browse/Play/Manage done")
				    (charterm-newline)
				    (done-ec))
				  (loop-fast-next-key))
				 (else (loop-fast-next-key))))
                             (begin
			       (if (message?)
				   (begin
				     (message? #f)
				     (draw-ui video-files
						  read-col-count
						  read-row-count)
				     (loop-fast-next-key))
				   (loop-maybe-check-screen-size))))))))))))))))


;; Prints the current version of massmine, and useful info
(define (print-version)
  (displayln (string-append "bpm: Browse, Play, Manage " bpm-version))
  (displayln "https://github.com/n3mo/bpm")
  (newline)
  (displayln "Copyright (C) 2017 Nicholas M. Van Horn")
  (displayln "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.")
  (displayln "This is free software: you are free to change and redistribute it.")
  (displayln "There is NO WARRANTY, to the extent permitted by law.")
  (exit 0))

;;; Command line parsing
(define filepath
  (command-line
   #:program "bpm"
   ;; #:once-each
   #:once-any
   [("-v" "--version") "Version info" (print-version)]
   #:args
   ([dir-path null])
   dir-path))


;;; Get things going
(define (main)
  ;; (let main-loop ([video-files (file-list)])
  ;;   (charterm-clear-screen)
  ;;   (charterm-cursor 1 1)
  ;;   (charterm-inverse)
  ;;   (charterm-display "File list:")
  ;;   (charterm-cursor 1 2)
  ;;   (charterm-normal)
  ;;   (for-each
  ;;    (λ (x)
  ;;      (charterm-display x)
  ;;      (charterm-newline))
  ;;    (take video-files 10)))
  (root-path filepath)
  (when (null? (root-path)) (root-path default-path))
  (run-bpm))


(main)
