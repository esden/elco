;;; Copyright (c) 2008, Piotr Esden-Tempski
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without 
;;; modification, are permitted provided that the following conditions are 
;;; met:
;;; 
;;; * Redistributions of source code must retain the above copyright notice, 
;;;   this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright 
;;;   notice, this list of conditions and the following disclaimer in the 
;;;   documentation and/or other materials provided with the distribution.
;;; * The names of its contributors may not be used to endorse or promote 
;;;   products derived from this software without specific prior written 
;;;   permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
;;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "elco-package.lisp")
(load "elco.lisp")

(defvar *poker-timer*)
(setf *poker-timer* 
      '(letrec ((clear_string (lambda (s i)
                                     (if (fx= (string-length s) i)
                                              s
                                              (progn (string-set s i #\Space)
                                                          (clear_string s (fx1+ i))))))
                     (fx_string (lambda (s fx i)
                                  (if (fx< fx 0)
                                           (progn
                                             (string-set s 0 #\-)
                                             (fx_string s (fx- 0 fx) i))
                                           (if (fxzerop i)
                                                    s
                                                    (progn
                                                      (string-set s i (fx-char (fx+ 48 (fxmod fx 10))))
                                                      (fx_string s (fx/ fx 10) (fx1- i)))))))
                     (pokertimer (lambda (sec blinds str txttime txtdot txtblinds txtspace txtend)
                                   (foreign-call "write" 0 txttime (string-length txttime))
                                   (clear_string str 0)
                                   (fx_string str (fx/ sec 3600) 9)
                                   (foreign-call "write" 0 str 10)
                                   (foreign-call "write" 0 txtdot (string-length txtdot))
                                   (clear_string str 0)
                                   (fx_string str (fxmod (fx/ sec 60) 60) 9)
                                   (foreign-call "write" 0 str 10)
                                   (foreign-call "write" 0 txtdot (string-length txtdot))
                                   (clear_string str 0)
                                   (fx_string str (fxmod sec 60) 9)
                                   (foreign-call "write" 0 str 10)
                                   (foreign-call "write" 0 txtblinds (string-length txtblinds))
                                   (clear_string str 0)
                                   (fx_string str (car (car blinds)) 9)
                                   (foreign-call "write" 0 str 10)                                   
                                   (foreign-call "write" 0 txtspace (string-length txtspace))
                                   (clear_string str 0)
                                   (fx_string str (car (cdr (car blinds))) 9)
                                   (foreign-call "write" 0 str 10)                                   
                                   (foreign-call "write" 0 txtend (string-length txtend))
                                   (foreign-call "sleep" 1)
                                   (pokertimer 
                                    (fx1+ sec)
                                    (if (fxzerop (fxmod (fx1+ sec) 120))
                                             (if (consp (cdr blinds))
                                                      (cdr blinds)
                                                      (cons (cons 
                                                                  (car (cdr (car blinds)))
                                                                  (cons
                                                                   (fx* 2 (car (cdr (car blinds))))
                                                                   nil))
                                                                 nil))
                                             blinds)
                                    str txttime txtdot txtblinds txtspace txtend))))
        (let ((str (make-string 10))
              (txttime (make-string 6))
              (txtdot (make-string 1))
              (txtblinds (make-string 9))
              (txtspace (make-string 1))
              (txtend (make-string 1))
              (blinds (cons (cons 1 (cons 2 nil))
                                 (cons
                                  (cons 2 (cons 4 nil))
                                  (cons
                                   (cons 5 (cons 10 nil))
                                   (cons
                                    (cons 10 (cons 20 nil))
                                    (cons
                                     (cons 25 (cons 50 nil))
                                     nil)))))))
          (string-set txttime 0 #\T)
          (string-set txttime 1 #\i)
          (string-set txttime 2 #\m)
          (string-set txttime 3 #\e)
          (string-set txttime 4 #\:)
          (string-set txttime 5 #\Space)

          (string-set txtdot 0 #\:)

          (string-set txtblinds 0 #\Space)
          (string-set txtblinds 1 #\B)
          (string-set txtblinds 2 #\l)
          (string-set txtblinds 3 #\i)
          (string-set txtblinds 4 #\n)
          (string-set txtblinds 5 #\d)
          (string-set txtblinds 6 #\s)
          (string-set txtblinds 7 #\:)
          (string-set txtblinds 8 #\Space)
          
          (string-set txtspace 0 #\Space)

          (string-set txtend 0 #\Return)

          (pokertimer 0 blinds str txttime txtdot txtblinds txtspace txtend)

          (clear_string str 0)
          (fx_string str -1 9)
          (foreign-call "write" 0 str 10))))

(elco:compile-program *poker-timer*)