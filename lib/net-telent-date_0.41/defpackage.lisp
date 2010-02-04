(defpackage :net.telent.date
  (:nicknames :date)
  (:use #:CL)
  (:export dayname monthname
	   with-date			; deprecated
	   with-decoding		; use this instead
	   second minute hour day-of-month month year day-of-week
           daylight-p zone universal-time-to-rfc-date	
	   universal-time-to-http-date
	   universal-time-to-rfc2822-date parse-time))
   
