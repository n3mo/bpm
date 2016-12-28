#! /usr/bin/env racket
#lang racket

;;; Dependencies
(require charterm)
(require racket/path)

;;; Parameters ----------------------------------------

;;; Current page of results
(define page (make-parameter 1))
;;; Cursor position
(define cursor (make-parameter 3))
;;; Row number of start of file listing
(define file-row (make-parameter 3))

;;; Files ending in the following extensions will be
;;; included in all operations. Add accordingly
(define valid-extensions
  '(#".flv" #".mov" #".mp4" #".wmv" #".mkv" #".mov" #".avi" #".mpg" #".swf"))

;;; The script expects a user-supplied target path at runtime.
;;; If no path is supplied, the following default is used. For
;;; more general audiences, this should probably be changed to "."
;; (define default-path "~/media/Television/")
(define default-path "~/main/work/teach/resources/intro_psych/videos/")

;;; Location of trash directory on this system. For now, this assumes
;;; that you're either using Mac OS X or linux...
(define trash-path
  (if (equal? 'macosx (system-type))
      "~/.Trash/"
      "~/.local/share/Trash/files/"))

;;; Find valid video files in directory (recursively)
(define (file-list [starting-path default-path])
  (find-files (λ (x) (member (path-get-extension x) valid-extensions))
	      (expand-user-path default-path)))


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
    (charterm-display (integer->char 2554))
    ;; Top edge of box
    (charterm-display (make-string width (integer->char 2550)))
    ;; Box corner upper-right
    (charterm-display (integer->char 2557))
    (charterm-newline)
    ;; Left side
    (for-each
     (λ (x)
       (charterm-cursor 1 x)
       (charterm-display (integer->char 2551)))
     (range 2 height))
    ;; Right side
    (for-each
     (λ (x)
       (charterm-cursor (+ width 1) x)
       ;; (charterm-display (integer->char 2551)))
       (charterm-display (integer->char 0001)))
     (range 2 height))

    ;; Box corner lower-left
    (charterm-cursor 1 height)
    (charterm-display (integer->char 2554))
    ;; Bottom edge of box
    (charterm-display (make-string width (integer->char 2550)))
    ;; Box corner upper-right
    (charterm-display (integer->char 2557))))


;;; Use this to determine which video file is listed first on the
;;; current page
(define (page-lead lst-length rows)
  (define max-vid (- rows 4))
  (let ((tmp (add1 (* (sub1 (page)) max-vid))))
    (if (> (+ tmp (sub1 max-vid)) lst-length)
	(- lst-length max-vid)
	tmp)))

;;; Displays video titles, along with any meta-data info
(define (display-titles lst cols rows)

  (define max-vid (- rows 4))
  ;;; (define max-vid 9) ;; debug
  
  ;; Don't let the user scroll past the last page of results
  ;; (when (> (* (page) max-vid) (length lst)) (page (sub1 (page))))
  (when (<= (page) 0) (page 1))
  (define start-point (page-lead (length lst) rows))

  ;; Correct the selection cursor if necessary
  (when (< (cursor) (file-row)) (cursor (file-row)))
  (when (> (cursor)
	   (+ (file-row) (length (drop lst start-point))))
    (cursor max-vid))
  
  (let loop ((pos (file-row)) (videos (drop lst start-point)))
    (charterm-cursor 8 pos)
    (charterm-clear-line-right)
    (if (= pos (cursor))
	(begin
	  (charterm-inverse)
	  (charterm-display
	   (path->string (file-name-from-path (car videos))))
	  (charterm-normal))
	(charterm-display
	 (path->string (file-name-from-path (car videos)))))
    (unless (= pos max-vid)
      (loop (+ pos 1) (cdr videos))))
  ;; Redraw UI interface around file list
  (charterm-cursor 3 2)
  (charterm-inverse)
  (charterm-display (format "File list (~A files) (page ~A)"
					       (length lst)
					       (page)))
  (charterm-normal)
  (draw-box cols rows))

;;; Call this whenever anything is updated
(define (draw-ui video-files cols rows)
  (display-titles video-files cols rows))

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

;; (define (%charterm:demo-input-redraw di)
;;   (charterm-cursor (%charterm:demo-input-x di)
;;                    (%charterm:demo-input-y di))
;;   (charterm-normal)
;;   (charterm-underline)
;;   (charterm-display (%charterm:demo-input-bytes di)
;;                     #:width (%charterm:demo-input-width di))
;;   (charterm-normal))

(define (run-bpm)
  (let ((data-row 12))
    (with-charterm
     (let ((ct (current-charterm)))
       (let/ec done-ec
         (let loop-remember-read-screen-size ((last-read-col-count 0)
                                              (last-read-row-count 0))

           (let loop-maybe-check-screen-size ((video-files (file-list)))
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
		     (charterm-clear-screen)
		     ;; (draw-box read-col-count read-row-count)
		     ;; (charterm-cursor 3 2)
		     ;; (charterm-inverse)
		     ;; (charterm-display (format "File list (~A files) (page ~A)"
		     ;; 			       (length video-files)
		     ;; 			       (page)))
		     ;; ;; (charterm-cursor 1 3)
		     ;; (charterm-normal)
		     (draw-ui video-files read-col-count read-row-count)		     

		     ;;(charterm-cursor 1 data-row)
		     ;;(charterm-display "To quit, press ")
		     ;;(charterm-bold)
		     ;;(charterm-display "Esc")
		     ;;(charterm-normal)
		     ;;(charterm-display ".")
		     
		     ;; (charterm-cursor 1 data-row)
		     ;; (charterm-insert-line)
		     ;; (charterm-display "termvar ")
		     ;; (charterm-bold)
		     ;; (charterm-display (charterm-termvar ct))
		     ;; (charterm-normal)
		     ;; (charterm-display ", protocol ")
		     ;; (charterm-bold)
		     ;; (charterm-display (charterm-protocol ct))
		     ;; (charterm-normal)
		     ;; (charterm-display ", keydec ")
		     ;; (charterm-bold)
		     ;; (charterm-display (charterm-keydec-id (charterm-keydec ct)))
		     ;; (charterm-normal)

		     ;; (charterm-cursor 1 data-row)
		     ;; (charterm-insert-line)
		     ;; (charterm-display #"Screen size: ")
		     ;; (charterm-bold)
		     ;; (charterm-display col-count)
		     ;; (charterm-normal)
		     ;; (charterm-display #" x ")
		     ;; (charterm-bold)
		     ;; (charterm-display row-count)
		     ;; (charterm-normal)
		     ;; (or read-screen-size?
		     ;; 	 (charterm-display #" (guessing; terminal would not tell us)"))

		     ;; (charterm-cursor 1 data-row)
		     ;; (charterm-insert-line)
		     ;; (charterm-display #"Widths:")
		     ;; (for-each (lambda (bytes)
		     ;; 		 (charterm-display #" [")
		     ;; 		 (charterm-underline)
		     ;; 		 (charterm-display bytes #:width 3)
		     ;; 		 (charterm-normal)
		     ;; 		 (charterm-display #"]"))
		     ;; 	       '(#"" #"a" #"ab" #"abc" #"abcd"))

		     ;; (and (eq? 'wy50 (charterm-protocol ct))
		     ;;      (begin
		     ;;        (charterm-cursor 1 data-row)
		     ;;        (charterm-insert-line)
		     ;;        (charterm-display #"Wyse WY-50 delete character: ab*c\010\010\eW")))

		     (loop-remember-read-screen-size read-col-count
						     read-row-count))
                   ;; Screen size didn't change (or we didn't check).
                   (begin
                     ;; (and clock-col
                     ;;      (begin (charterm-inverse)
                     ;;             (charterm-cursor clock-col 1)
                     ;;             (charterm-display (parameterize ((date-display-format 'iso-8601))
                     ;;                                 (substring (date->string (current-date) #t)
                     ;;                                            11)))
                     ;;             (charterm-normal)))

                     (let loop-fast-next-key ()
                       ;; (%charterm:demo-input-put-cursor di)
                       (let ((keyinfo (charterm-read-keyinfo #:timeout 1)))
                         (if keyinfo
                             (let ((keycode (charterm-keyinfo-keycode keyinfo)))
                               (charterm-cursor 1 (sub1 read-row-count))
                               (charterm-insert-line)
                               (charterm-display "Read key: ")
                               (charterm-bold)
                               (charterm-display (or (charterm-keyinfo-keylabel keyinfo) "???"))
                               (charterm-normal)
                               (charterm-display (format " ~S"
                                                         `(,(charterm-keyinfo-keyset-id    keyinfo)
                                                           ,(charterm-keyinfo-bytelang     keyinfo)
                                                           ,(charterm-keyinfo-bytelist     keyinfo)
                                                           ,@(charterm-keyinfo-all-keycodes keyinfo))))
                               (case keycode				 
				 ((left)
				  (begin (page (sub1 (page)))
					 (draw-ui video-files
						  read-col-count
						  read-row-count))
				  (loop-fast-next-key))
				 ((right)
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
                               ;; (charterm-display "Timeout.")
                               (loop-maybe-check-screen-size video-files)))))))))))))))


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
  (run-bpm))


(main)
