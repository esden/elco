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
      '(elco:letrec ((clear_string (elco:lambda (s i)
                                     (elco:if (elco:fx= (elco:string-length s) i)
                                              s
                                              (elco:progn (elco:string-set s i #\Space)
                                                          (clear_string s (elco:fx1+ i))))))
                     (fx_string (elco:lambda (s fx i)
                                  (elco:if (elco:fx< fx 0)
                                           (elco:progn
                                             (elco:string-set s 0 #\-)
                                             (fx_string s (elco:fx- 0 fx) i))
                                           (elco:if (elco:fxzerop i)
                                                    s
                                                    (elco:progn
                                                      (elco:string-set s i (elco:fx-char (elco:fx+ 48 (elco:fxmod fx 10))))
                                                      (fx_string s (elco:fx/ fx 10) (elco:fx1- i)))))))
                     (pokertimer (elco:lambda (sec blinds str txttime txtdot txtblinds txtspace txtend)
                                   (elco:foreign-call "write" 0 txttime (elco:string-length txttime))
                                   (clear_string str 0)
                                   (fx_string str (elco:fx/ sec 3600) 9)
                                   (elco:foreign-call "write" 0 str 10)
                                   (elco:foreign-call "write" 0 txtdot (elco:string-length txtdot))
                                   (clear_string str 0)
                                   (fx_string str (elco:fxmod (elco:fx/ sec 60) 60) 9)
                                   (elco:foreign-call "write" 0 str 10)
                                   (elco:foreign-call "write" 0 txtdot (elco:string-length txtdot))
                                   (clear_string str 0)
                                   (fx_string str (elco:fxmod sec 60) 9)
                                   (elco:foreign-call "write" 0 str 10)
                                   (elco:foreign-call "write" 0 txtblinds (elco:string-length txtblinds))
                                   (clear_string str 0)
                                   (fx_string str (elco:car (elco:car blinds)) 9)
                                   (elco:foreign-call "write" 0 str 10)                                   
                                   (elco:foreign-call "write" 0 txtspace (elco:string-length txtspace))
                                   (clear_string str 0)
                                   (fx_string str (elco:car (elco:cdr (elco:car blinds))) 9)
                                   (elco:foreign-call "write" 0 str 10)                                   
                                   (elco:foreign-call "write" 0 txtend (elco:string-length txtend))
                                   (elco:foreign-call "sleep" 1)
                                   (pokertimer 
                                    (elco:fx1+ sec)
                                    (elco:if (elco:fxzerop (elco:fxmod (elco:fx1+ sec) 120))
                                             (elco:if (elco:consp (elco:cdr blinds))
                                                      (elco:cdr blinds)
                                                      (elco:cons (elco:cons 
                                                                  (elco:car (elco:cdr (elco:car blinds)))
                                                                  (elco:cons
                                                                   (elco:fx* 2 (elco:car (elco:cdr (elco:car blinds))))
                                                                   elco:nil))
                                                                 elco:nil))
                                             blinds)
                                    str txttime txtdot txtblinds txtspace txtend))))
        (let ((str (elco:make-string 10))
              (txttime (elco:make-string 6))
              (txtdot (elco:make-string 1))
              (txtblinds (elco:make-string 9))
              (txtspace (elco:make-string 1))
              (txtend (elco:make-string 1))
              (blinds (elco:cons (elco:cons 1 (elco:cons 2 elco:nil))
                                 (elco:cons
                                  (elco:cons 2 (elco:cons 4 elco:nil))
                                  (elco:cons
                                   (elco:cons 5 (elco:cons 10 elco:nil))
                                   (elco:cons
                                    (elco:cons 10 (elco:cons 20 elco:nil))
                                    (elco:cons
                                     (elco:cons 25 (elco:cons 50 elco:nil))
                                     elco:nil)))))))
          (elco:string-set txttime 0 #\T)
          (elco:string-set txttime 1 #\i)
          (elco:string-set txttime 2 #\m)
          (elco:string-set txttime 3 #\e)
          (elco:string-set txttime 4 #\:)
          (elco:string-set txttime 5 #\Space)

          (elco:string-set txtdot 0 #\:)

          (elco:string-set txtblinds 0 #\Space)
          (elco:string-set txtblinds 1 #\B)
          (elco:string-set txtblinds 2 #\l)
          (elco:string-set txtblinds 3 #\i)
          (elco:string-set txtblinds 4 #\n)
          (elco:string-set txtblinds 5 #\d)
          (elco:string-set txtblinds 6 #\s)
          (elco:string-set txtblinds 7 #\:)
          (elco:string-set txtblinds 8 #\Space)
          
          (elco:string-set txtspace 0 #\Space)

          (elco:string-set txtend 0 #\Return)

          (pokertimer 0 blinds str txttime txtdot txtblinds txtspace txtend)

          (clear_string str 0)
          (fx_string str -1 9)
          (elco:foreign-call "write" 0 str 10))))

(elco:compile-program *poker-timer*)