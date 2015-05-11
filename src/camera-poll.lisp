(in-package :spies-asd)

;;; SPIRE Event Service

;; Server polling

;; Polling JSON information from suppliers over two different interfaces.

					; mikko.rinne@aalto.fi 13.11.2014
; Modified by MJR 11.5.2015: Replaced addresses & passes with placeholders and inserted INIT_CONFIG tags to mark the lines, where something needs to be done.

; Parameters related to polls:
(defvar *camerasensor-source* "camerasensor@camerasensor.fi") ; An arbitrary email-looking address to register with occupancy updates
(defvar *loopsensor-source* "loopsensor@loopsensor.fi")
(defparameter camerasensor-uri "http://0.0.0.0/getevents?" ; INIT_CONFIG: Fill in the URI to poll for the camera sensor
"Base URI for polling the camera sensor data from camerasensor") ; URI for Camerasensor polls. Variable because SPIRE also provided a test interface
(defparameter camerasensor-picture "http://0.0.0.0/events/picture.jpg" ; INIT_CONFIG: Fill in the address to obtain picture from the camera sensor
"Base URI for polling the latest camera image from camerasensor")
(defparameter camerasensor-key "XXXXXXXXXXXXXXXXXXX") ; INIT_CONFIG: Fill-in the key for the camera sensor supplier
(defparameter loopsensor-uri "https://0.0.0.0/admin/jobs/export") ; INIT_CONFIG: Fill in the URI for polling from loop sensor
(defparameter loopsensor-username "username") ; INIT_CONFIG: Fill in the username for the loop sensor
(defparameter loopsensor-pass "XXXXXXXXXXXXXXX") ; INIT_CONFIG: Fill in the password for the loop sensor

(defparameter occupancy-change-uri "http://127.0.0.1:4242/spire/relativeoccupancyupdate?" 
"Base URI for updating changes to spies server.")
; The URI above is ok when the SPIRE server and camera poll are running in the same machine.

; Some global variables to hold values during execution:
(defvar *lastcounter* "0") ; Replaced in sensor-poll-sequence
(defvar *lastdate* "2001-01-30") ; Replaced in sensor-poll-sequence
(defvar *loopsensor-request-date* "01/30/2001") ; Replaced in sensor-poll-sequence
(defvar *sensorcounts* nil) ; Collect name <-> relative-change pairs for each area

;;; ********************************
;;; Sensor Poll Sequence
;;; ********************************

; Eternal loop to keep polling with the specified interval
; Optional parameters: Starting date (default: current date) and last received counter (default: "0")
; Note1: Both the date and the counter have to be strings!!
; Note2: Slime aborts with C-c C-c, for regular sbcl one C-c is enough.

(defun sensor-poll-sequence (&optional (start-date (subseq (datetime-now) 0 10)) (start-lastreceived "0"))

; These settings are replaced by automatically obtaining the latest counter and time values, 
; so polling will start from the current moment. Code saved here in case someone wants to parametrize this again.
;  (setq *lastdate* start-date)
;  (setq *lastcounter* start-lastreceived)
; Use the designated startdate and the current time to initialize Loopsensor - Loopsensor is in UTC so this will probably give a few hours worth of events.
;  (setq *loopsensor-request-date* (concatenate 'string (date-to-loopsensor-request *lastdate*) ":" (subseq (datetime-now) 11)))

  (camerasensor-set-last-date-counter camerasensor-uri) ; Set Camerasensor to current date and last counter currently available
  (loopsensor-set-last-time) ; Set the latest timestamp for Loopsensor
  (sleep 60) ; Wait a minute - otherwise the first poll will certainly be empty
  (loop
     (camerasensor-poll camerasensor-uri)
     (loopsensor-poll)
     (format t "~A::  Camerasensor last received counter: ~A Loopsensor time: ~A~%" (datetime-now) *lastcounter* *loopsensor-request-date*)
     (sleep (* 5 60)) ;sleep takes seconds, so x minutes * 60 seconds
;     (sleep 10) ; faster loop for debugging
     )
  )

;;; **********
;;; Camerasensor poll
;;; **********

(defun camerasensor-poll (sensor-poll-uri)
  (let ((sensor-response (parse-simple-json-response
			  (concatenate 'string sensor-poll-uri "lastcounter=" *lastcounter* "&date=" *lastdate* "&key=" camerasensor-key))))
					; Check that there is at least one element in the response
    (if (> (list-length sensor-response) 0)
	(progn
	  (with-open-file (fhandle "camerasensor_input_log.txt" ; INIT_CONFIG: The name of the camera sensor input log file.
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)
	    (reset-count) ; Clean up the observation count list
	    (loop for item in sensor-response do
					; Write each item to file as a separate line
		 (format fhandle (concatenate 'string (json:encode-json-to-string item) "~%"))
		 (let ((event-in (string-downcase (cdr (assoc :event item))))
		       (sensor-in (cdr (assoc :sensor item)))
		       (gate-in (cdr (assoc :gate item))))
		   (setq *lastcounter* (cdr (assoc :counter item)))
		   (setq *lastdate* (subseq (cdr (assoc :datetime item)) 0 10))
					; A quick-and-dirty solution to separate parking areas depending on gate 5 (OIH) vs. other gates (TUAS)
		   (if (equalp gate-in "5") (setq sensor-in "Camera1-OIH") (setq sensor-in "Camera1-TUAS"))
		   (if (equalp event-in "car enters") (update-count sensor-in 1))
		   (if (equalp event-in "car exits") (update-count sensor-in -1))
		   ) ) ;end-loop for JSON processing
	    ) ;close input logfile
	  (write-sensor-results *camerasensor-source*) ; Write result to log & post to SPIRE server
	  ; (camerasensor-save-picture) ; Save the latest picture from Camerasensor camera (commented out to decrease disk space usage)
	  ) ;end-of-progn
	) ;end-of-if
    ); end-of-let
  )

; Macro to define awhile from: http://dunsmor.com/lisp/onlisp/onlisp_18.html#SEC100
; MJR: SBCL seems to have awhile too, but it didn't work the same way ("it" not recognized) and I didn't find documentation for it.
(defmacro awhile (expr &body body)
        `(do ((it ,expr ,expr))
                ((not it))
             ,@body))

(defvar it) ; trying to avoid the "undefined"-problems

; Directly from: http://stackoverflow.com/questions/12607210/how-to-download-and-save-a-file-using-drakmahttp-request-and-flexistreams
(defun camerasensor-save-picture ()
  (let ((input (handler-case (drakma:http-request camerasensor-picture :want-stream t)
		 (error (err-message) (format t "Address: ~A returned error: ~A~%" camerasensor-picture err-message)))))
; TODO: Prevent opening the file if the request was unsuccessful
    (with-open-file (file (concatenate 'string "images/Camera1-" *lastdate* "-" *lastcounter* ".jpg")
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
      (awhile (read-byte input nil nil)
	(write-byte it file))
      (close input))))


;;; ************
;;; Loopsensor poll
;;; ************

(defun loopsensor-poll ()
					; NOTE: Request date format is American: Month/Date/Year
  (let ((sensor-response (cl-csv:read-csv (handler-case (drakma:http-request loopsensor-uri
									     :method :post
									     :basic-authorization (list loopsensor-username loopsensor-pass)
									     :content (concatenate 'string "search=search index=trafficmonitor earliest=\"" *loopsensor-request-date* "\" latest=now&output_mode=csv"))
; TODO: Figure out why an error on the next line freezes the polling? Should sensor-response be reset to NIL, just in case something ugly comes with an error?
					    (error (err-message) (format t "Address: ~A returned error: ~A~%" loopsensor-uri err-message))))))
    (if (> (list-length sensor-response) 1) ; assuming here that the header row will come also with an empty message
	(progn
	  (with-open-file (fhandle "loopsensor_input_log.txt" ; INIT_CONFIG: Name of the loopsensor input log.
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)
	    (reset-count)
	    (loop for item in (cdr sensor-response) do
		 (let ((parsed (comma-split (substitute #\, #\| (nth 7 item)))) ; Create a list like: ("S5" "out" "time: 10.10.2014 16:58:04")
		       (change 1)) ; Corresponds to "in"
		   (if (equalp (nth 1 parsed) "out") (setq change -1))
		   (format fhandle "~{~A~^,~}~%" parsed) ; Write a comma-separated line of the key parameters to input log
					; Calculate the total change per parking area
		   (if (equalp (nth 0 parsed) "S1") (update-count "Loop-P1" change))
		   (if (equalp (nth 0 parsed) "S6") (update-count "Loop-P2" change))
		   (if (equalp (nth 0 parsed) "S3") (progn 
						      (update-count "Loop-P4" change)
						      (update-count "Loop-P5" (- change))))
		   (if (equalp (nth 0 parsed) "S2") (update-count "Loop-P5" change))
		   (if (equalp (nth 0 parsed) "S4") (update-count "Loop-P5" (- change)))
		   )) ; End input processing loop
					; Find the last time in the list (comes in the first entry) and convert to seconds
	    (let* ((in-time (subseq (car (last (spies-asd::comma-split (substitute #\, #\| (nth 7 (car (cdr sensor-response))))))) 6))
		   (year (parse-integer (subseq in-time 6 10)))
		   (month (parse-integer (subseq in-time 3 5)))
		   (day (parse-integer (subseq in-time 0 2)))
		   (hour (parse-integer (subseq in-time 11 13)))
		   (minute (parse-integer (subseq in-time 14 16)))
		   (second (parse-integer (subseq in-time 17)))
		   (next-universal-time (+ (encode-universal-time second minute hour day month year) 1))) ; Advance time by one second from the last one
					; Update the loopsensor request time to correspond to the next second 
	      (setq *loopsensor-request-date* (multiple-value-bind
						 (second minute hour day month year) (decode-universal-time next-universal-time)
					     (format nil "~2,'0d/~2,'0d/~d:~2,'0d:~2,'0d:~2,'0d"
						     month day year hour minute second))))
	    ) ; close input logfile
	  (write-sensor-results *loopsensor-source*)

	  ))))

;;; ********************
;;; Write sensor results
;;; ********************

(defun write-sensor-results (source-email)
  (with-open-file (csvhandle "processed_log.csv" ; INIT_CONFIG: Name of the log file written from all inputs
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
    (loop for sensor-count in *sensorcounts* do
	 (let ((sensor-name (car sensor-count))
	       (sensor-change (cdr sensor-count)))
	   (if (not (= sensor-change 0))
	       (progn
		 (let ((occupancy-response 
			(car (cdr (assoc :bindings
					 (cdr (assoc :results
						     (parse-simple-json-response (concatenate 'string occupancy-change-uri "source=" source-email "&sensor=" sensor-name "&change=" (write-to-string sensor-change))))))))))
		   (format csvhandle "~A,\"~A\",~A,~A,~A,~A~%"
			   (datetime-now)
			   sensor-name
			   sensor-change
			   (extract-sparql-value occupancy-response :occupied-spaces)
			   (extract-sparql-value occupancy-response :size)
			   (extract-sparql-value occupancy-response :status))))))) ; end-of-sensorcount-loop
    ) ;close csv logfile
  )

;;; *******************
;;; Auxiliary functions
;;; *******************

(defun parse-simple-json-response (sensor-uri-str)
  (handler-case
      (cl-json:decode-json-from-string (flexi-streams:octets-to-string (drakma:http-request sensor-uri-str) :external-format :utf-8))
    (error (err-message) (format t "Address: ~A returned error: ~A~%" sensor-uri-str err-message)))
  )

; Reset the counter used for counting deltas for individual sensors
(defun reset-count ()
  (setq *sensorcounts* nil) ; Clean up the observation count list
  )

; Update the counter for a certain key
(defun update-count (key change)
  (let ((current-pair (assoc key *sensorcounts* :test #'equalp))
	)
    (if current-pair
	(rplacd (assoc key *sensorcounts* :test #'equalp) (+ (cdr current-pair) change)) ; then update value
	(setq *sensorcounts* (acons key change *sensorcounts*)) ; else add new element
	)
    )
  )

; Return the current counter value of a certain key
(defun current-count (key)
  (cdr (assoc key *sensorcounts* :test #'equalp)))


; Convert an xsd-format date (2001-01-30) to the format used in loopsensor requests (01/30/2001)
(defun date-to-loopsensor-request (date)
  (concatenate 'string (subseq date 5 7) "/" (subseq date 8 10) "/" (subseq date 0 4))
)

; Obtain the latest counter of today from Camerasensor
(defun camerasensor-set-last-date-counter (sensor-poll-uri)
  (let ((sensor-response (parse-simple-json-response
			  (concatenate 'string sensor-poll-uri "lastcounter=0&date=" (subseq (datetime-now) 0 10) "&key=" camerasensor-key))))
    (if (> (list-length sensor-response) 0)
	(progn
	  (let ((item (car (last sensor-response))))
	    (setq *lastcounter* (cdr (assoc :counter item)))
	    (setq *lastdate* (subseq (cdr (assoc :datetime item)) 0 10)))
	  ) ;end-of-progn
	(progn ; else: No events have been created today - wait for the first one
	  (setq *lastcounter* "0")
	  (setq *lastdate* (subseq (datetime-now) 0 10))
	  ) ;end-of-progn
	) ;end-of-if
    ); end-of-let
  )


; Obtain the last timestamp in current Loopsensor response
(defun loopsensor-set-last-time ()
  (let ((sensor-response (cl-csv:read-csv (handler-case
					      (drakma:http-request loopsensor-uri
								   :method :post
								   :basic-authorization (list loopsensor-username loopsensor-pass)
								   :content (concatenate 'string "search=search index=trafficmonitor | head 1&output_mode=csv"))
					    (error (err-message) (format t "Address: ~A returned error: ~A~%" loopsensor-uri err-message))))))
    (if (> (list-length sensor-response) 1) ; assuming here that the header row will come also with an empty message
	(progn
					; Find the last time in the list
	  (let* ((in-time (subseq (car (last (spies-asd::comma-split (substitute #\, #\| (nth 7 (car (cdr sensor-response))))))) 6))
		 (year (parse-integer (subseq in-time 6 10)))
		 (month (parse-integer (subseq in-time 3 5)))
		 (day (parse-integer (subseq in-time 0 2)))
		 (hour (parse-integer (subseq in-time 11 13)))
		 (minute (parse-integer (subseq in-time 14 16)))
		 (second (parse-integer (subseq in-time 17)))
		 (next-universal-time (+ (encode-universal-time second minute hour day month year) 1))) ; Advance time by one second from the last one
					; Update the loopsensor request time to correspond to the next second 
	    (setq *loopsensor-request-date* (multiple-value-bind
					       (second minute hour day month year) (decode-universal-time next-universal-time)
					   (format nil "~2,'0d/~2,'0d/~d:~2,'0d:~2,'0d:~2,'0d"
						   month day year hour minute second))))
	  ))))

