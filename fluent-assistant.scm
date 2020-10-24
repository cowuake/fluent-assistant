;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLUENT SCHEME ASSISTANT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2020 Riccardo Mura
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     1. Redistributions of source code must retain the above copyright notice,
;;        this list of conditions and the following disclaimer.
;;     2. Redistributions in binary form must reproduce the above copyright
;;        notice, this list of conditions and the following disclaimer in the
;;        documentation and/or other materials provided with the distribution.
;;     3. Neither the name of the copyright holder nor the names of its
;;        contributors may be used to endorse or promote products derived from
;;        this software without specific prior written permission.
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL VARIABLE DEFINITIONS
;;             (FLUENT SPECIFIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default name for generated temp files
(define default-temp-output
  "fluent-scheme-temp")


;(define save-file-list
;  (lambda ()
;    (let exec-string
;	(string-append "ls > " default-temp-output)
;      (system exec-string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTACTIC EXTENSIONS
;;      (NOT SO REUSABLE)
;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-syntax when
;  (syntax-rules ()
;    ((_ condition body ...)
;     (if condition body ...))))


;; TODO Redefine the obsolete macro system
;(syntax-table-define system-global-syntax-table 'old-style-macro
;  (macro (list name args body)
;    (list 'syntax-table-define
;	  'system-global-syntax-table
;	  (quote name)
;	  (list 'macro
;		args
;		body))))


(syntax-table-define system-global-syntax-table 'when
  (macro (condition . body)
    (list 'if condition
	  (cons 'begin body))))


;(old-style-macro 'when
;		 '(condition . body)
;		 (list 'if condition
;		       (cons 'begin body)))


(syntax-table-define system-global-syntax-table 'unless
  (macro (condition . body)
    (list 'if
	  (list 'not condition)
	  (cons 'begin body))))



;;;;;;;;;;;;;;;;;;;;;
;; CFD MISCELLANEA
;;        (REUSABLE)
;;;;;;;;;;;;;;;;;;;;;

(define sst-beta 0.09)
(define sst-beta* 0.0828)


(define sst-turbulence-decay
  (lambda (tu-inlet rho mu evr u x tu)
    (- (sqrt (* (sqr tu-inlet)
		(** (add1 (/ (* 3 rho u x sst-beta (sqr tu-inlet))
			     (* 2 mu evr)))
		    (- (/ sst-beta* sst-beta)))))
       tu)))


(define compute-transition-sst-inlet-tu
  (lambda (rho mu evr u x tu)
    (define sst-turbulence-decay-f
      (lambda (tu-inlet)
	(sst-turbulence-decay tu-inlet rho mu evr u x (/ tu 100))))
    (let ((result ( * (car (illinois sst-turbulence-decay-f 0 100))
		     100)))
      (if (and (< result 100)
	       (> result 0))
	  result
	  (error "Calculated an invalid number")))))



;;;;;;;;;;;;;;;;;;;;;;;
;; GENERIC UTILITIES
;;   (FLUENT SPECIFIC)
;;;;;;;;;;;;;;;;;;;;;;;

(define reload-fluent-scheme
  (lambda ()
    (begin
      (ti-menu-load-string "file read-macros fluent-assistant.scm")
      (ti-menu-load-string "file read-macros portable-core.scm"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLUENT SOLVER SETTINGS
;;        (FLUENT SPECIFIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Association list of discretization schemes for the convective terms of the equations
(define convective-discretization-schemes
  '(
    ("First Order Upwind"   . 0)
    ("Second Order Upwind"  . 1)
    ("Power Law"            . 2)
    ("Central Differencing" . 3)
    ("QUICK"                . 4)
    ("Third-Order MUSCL"    . 6)))


(define pressure-coupling-algorithms
  '(
    ("SIMPLE"  . 20)
    ("SIMPLEC" . 21)
    ("PISO"    . 22)
    ("Coupled" . 24)))


(define pressure-discretization-schemes
  '(
    ("Second Order"        . 12)
    ("Standards"           . 10)
    ("PRESTO!"             . 14)
    ("Linear"              . 11)
    ("Body Force Weighted" . 13)))


(define set-indexed-option
  (lambda (command target option alist)
    (ti-menu-load-string (string-append command
					target
					(number->string
					 (get-value-from-alist option alist))
					"\n"))))


(define set-discretization-scheme
  (lambda (discretization-scheme target alist)
    (set-indexed-option "solve set discretization-scheme "
			target
			discretization-scheme
			alist)))


(define set-momentum-discretization-scheme
  (lambda (discretization-scheme)
    (set-discretization-scheme discretization-scheme
			       "mom "
			       convective-discretization-schemes)))


(define set-pressure-discretization-scheme
  (lambda (discretization-scheme)
    (set-discretization-scheme discretization-scheme
			       "pressure "
			       pressure-discretization-schemes)))


(define set-pressure-coupling-algorithm
  (lambda (option)
    (set-indexed-option "solve set p-v-coupling "
			""
			option
			pressure-coupling-algorithms)))


(define set-gradient-scheme
  (lambda (scheme)
    (cond
     ((equal?  scheme "Green-Gauss Node Based")
      (ti-menu-load-string "solve set gradient-scheme \n yes"))
     ((equal?  scheme "Green-Gauss Cell Based")
      (ti-menu-load-string "solve set gradient-scheme \n no \n no \n yes"))
     ((equal?  scheme "Least-Squares")
      (ti-menu-load-string "solve set gradient-scheme \n no \n yes")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESH-RELATED UTILITIES
;;       (FLUENT SPECIFIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rename-surface
  (lambda (old-name new-name)
    (ti-menu-load-string (string-append
			  "surface rename-surface "
			  old-name
			  " "
			  new-name
			  "\n"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA IMPORT / EXPORT / TRANSLATION
;;                    (FLUENT SPECIFIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define import-profile-from-raw-text
  (lambda (file-name)
    (let* ((raw-text (read-lines file-name))
	   (header (string-append "(("
				  (car raw-text)
				  ")"))
	   (fields (cdr raw-text))
	   (body (string-list->string (transpose (map parse-csv-line fields)))))
      (append-to-file (append (list header)
			      (parse-line (string-append (substring body
								    1
								    (- (string-length body)
								       1))
							 ")")
					  #\) #f))
		      "profiles.scm"))))


(define load-profiles
  (lambda ()
    (let ((file-name "profiles.scm"))
      (if (file-exists? file-name)
	  (ti-read-profile file-name)
	  (display (string-append "Impossible to import profiles. File "
				  file-name
				  " not found."))))))



;;;;;;;;;;;;;;;;
;; TEST SUITE
;; (FLUENT SPECIFIC)
;;;;;;;;;;;;;;;;

(define test-fluent-scheme
  (lambda ()
    (begin
       (ti-read-case "test.cas")
       (set-momentum-discretization-scheme "QUICK")
       (set-pressure-discretization-scheme "PRESTO!")
       (set-pressure-coupling-algorithm "Coupled")
       (set-gradient-scheme "Least-Squares")
       (load-profiles)
       (read-lines "profile.raw")
       (merge-strings (parse-csv-line
		       "This,was,a,long,string,with,many,commas")
		      "=//")
       (import-profile-from-raw-text "profile.raw")
       (load-profiles "profiles.scm")
       )))


(ti-menu-load-string "file read-macros portable-core.scm")
