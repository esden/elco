;;; Copyright (c) 2008, Piotr Esden-Tempski <piotr at esden.net>
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

(in-package :elco-test)

(defvar *all-tests* '())

(defun run-test (input output)
  (elco:compile-program input)
  (let ((gcc-output (elco:build)))
    (if (not (equal gcc-output ""))
        (format t "gcc problem: ~s~%" gcc-output)
        (let ((elco-output (elco:execute)))
          (if (not (equal elco-output (format nil output)))
              (format t "FAILED, expected output was ~s but got ~s~%" output elco-output)
              t)))))

(defun run-tests (&key tests select)
  (if tests
      (if (stringp (car tests))
          (let ((break-loop nil))
            (format t "Running testsuite for ~s~%" (car tests))
            (force-output *standard-output*)
            (loop for i in (cadr tests)
               until break-loop do
               (format t "test ~s : ~s ... " (car tests) i)
               (if (run-test (car i) (caddr i))
                   (format t "PASSED~%")
                   (setf break-loop T)))
            (if break-loop
                nil
                (progn 
                  (format t "All tests succeeded!~%")
                  t)))
          (loop for i in tests  
             until (if select 
                       (if (equal (car i) select)
                           (not (run-tests :tests i))
                           nil)
                       (not (run-tests :tests i)))))
      (if select
          (run-tests :select select :tests *all-tests*)
          (run-tests :tests *all-tests*))))

(defmacro deftest ((test-id) &body body)
  `(setf *all-tests* 
         (append *all-tests* 
                 (list (list
                        ,test-id
                        ',body)))))

(setf *all-tests* '())

;;; Step 1

(deftest ("fixnum")
  (0  => "0~%")
  (1  => "1~%")
  (-1 => "-1~%")
  (10  => "10~%")
  (-10 => "-10~%")                   
  (2736 => "2736~%")             
  (-2736 => "-2736~%")
  (536870911 => "536870911~%")
  (-536870912 => "-536870912~%"))

;;; Step 2

(deftest ("immediate constants")
  (F => "F~%")
  (T => "T~%")
  (nil => "NIL~%")
  (#\Nul => "#\\nul~%")
  (#\Soh => "#\\soh~%")
  (#\Stx => "#\\stx~%")
  (#\Etx => "#\\etx~%")
  (#\Eot => "#\\eot~%")
  (#\Enq => "#\\enq~%")
  (#\Ack => "#\\ack~%")
  (#\Bel => "#\\bel~%")
  (#\Backspace => "#\\bs~%")
  (#\tab => "#\\tab~%")
  (#\newline => "#\\newline~%")
  (#\Vt => "#\\vt~%")
  (#\Page => "#\\ff~%")
  (#\Return => "#\\return~%")
  (#\So => "#\\so~%")
  (#\Si => "#\\si~%")
  (#\Dle => "#\\dle~%")
  (#\Dc1 => "#\\dc1~%")
  (#\Dc2 => "#\\dc2~%")
  (#\Dc3 => "#\\dc3~%")
  (#\Dc4 => "#\\dc4~%")
  (#\Nak => "#\\nak~%")
  (#\Syn => "#\\syn~%")
  (#\Etb => "#\\etb~%")
  (#\Can => "#\\can~%")
  (#\Em => "#\\em~%")
  (#\Sub => "#\\sub~%")
  (#\Esc => "#\\esc~%")
  (#\Fs => "#\\fs~%")
  (#\Gs => "#\\gs~%")
  (#\Rs => "#\\rs~%")
  (#\Us => "#\\us~%")
  (#\space => "#\\space~%")
  (#\! => "#\\!~%")
  (#\" => "#\\\"~%")
  (#\# => "#\\#~%")
  (#\$ => "#\\$~%")
  (#\% => "#\\%~%")
  (#\& => "#\\&~%")
  (#\' => "#\\'~%")
  (#\( => "#\\(~%")
  (#\) => "#\\)~%")
  (#\* => "#\\*~%")
  (#\+ => "#\\+~%")
  (#\, => "#\\,~%")
  (#\- => "#\\-~%")
  (#\. => "#\\.~%")
  (#\/ => "#\\/~%")
  (#\0 => "#\\0~%")
  (#\1 => "#\\1~%")
  (#\2 => "#\\2~%")
  (#\3 => "#\\3~%")
  (#\4 => "#\\4~%")
  (#\5 => "#\\5~%")
  (#\6 => "#\\6~%")
  (#\7 => "#\\7~%")
  (#\8 => "#\\8~%")
  (#\9 => "#\\9~%")
  (#\: => "#\\:~%")
  (#\; => "#\\;~%")
  (#\< => "#\\<~%")
  (#\= => "#\\=~%")
  (#\> => "#\\>~%")
  (#\? => "#\\?~%")
  (#\@ => "#\\@~%")
  (#\A => "#\\A~%")
  (#\B => "#\\B~%")
  (#\C => "#\\C~%")
  (#\D => "#\\D~%")
  (#\E => "#\\E~%")
  (#\F => "#\\F~%")
  (#\G => "#\\G~%")
  (#\H => "#\\H~%")
  (#\I => "#\\I~%")
  (#\J => "#\\J~%")
  (#\K => "#\\K~%")
  (#\L => "#\\L~%")
  (#\M => "#\\M~%")
  (#\N => "#\\N~%")
  (#\O => "#\\O~%")
  (#\P => "#\\P~%")
  (#\Q => "#\\Q~%")
  (#\R => "#\\R~%")
  (#\S => "#\\S~%")
  (#\T => "#\\T~%")
  (#\U => "#\\U~%")
  (#\V => "#\\V~%")
  (#\W => "#\\W~%")
  (#\X => "#\\X~%")
  (#\Y => "#\\Y~%")
  (#\Z => "#\\Z~%")
  (#\( => "#\\(~%")
  (#\\ => "#\\\\~%")
  (#\] => "#\\]~%")
  (#\^ => "#\\^~%")
  (#\_ => "#\\_~%")
  (#\` => "#\\`~%")
  (#\a => "#\\a~%")
  (#\b => "#\\b~%")
  (#\c => "#\\c~%")
  (#\d => "#\\d~%")
  (#\e => "#\\e~%")
  (#\f => "#\\f~%")
  (#\g => "#\\g~%")
  (#\h => "#\\h~%")
  (#\i => "#\\i~%")
  (#\j => "#\\j~%")
  (#\k => "#\\k~%")
  (#\l => "#\\l~%")
  (#\m => "#\\m~%")
  (#\n => "#\\n~%")
  (#\o => "#\\o~%")
  (#\p => "#\\p~%")
  (#\q => "#\\q~%")
  (#\r => "#\\r~%")
  (#\s => "#\\s~%")
  (#\t => "#\\t~%")
  (#\u => "#\\u~%")
  (#\v => "#\\v~%")
  (#\w => "#\\w~%")
  (#\x => "#\\x~%")
  (#\y => "#\\y~%")
  (#\z => "#\\z~%")
  (#\{ => "#\\{~%")
  (#\| => "#\\|~%")
  (#\} => "#\\}~%")
  (#\~ => "#\\~~~%")
  (#\Rubout => "#\\del~%"))

;;; Step 3

(deftest ("fx1+")
  ((fx1+ 0) => "1~%")
  ((fx1+ -1) => "0~%")
  ((fx1+ 1) => "2~%")
  ((fx1+ -100) => "-99~%")
  ((fx1+ 1000) => "1001~%")
  ((fx1+ 536870910) => "536870911~%")
  ((fx1+ -536870912) => "-536870911~%")
  ((fx1+ (fx1+ 0)) => "2~%")
  ((fx1+ (fx1+ (fx1+ (fx1+ (fx1+ (fx1+ 12)))))) => "18~%"))

(deftest ("fx-char and char-fx")
   ((fx-char 65) => "#\\A~%")
   ((fx-char 97) => "#\\a~%")
   ((fx-char 122) => "#\\z~%")
   ((fx-char 90) => "#\\Z~%")
   ((fx-char 48) => "#\\0~%")
   ((fx-char 57) => "#\\9~%")
   ((char-fx #\A) => "65~%")
   ((char-fx #\a) => "97~%")
   ((char-fx #\z) => "122~%")
   ((char-fx #\Z) => "90~%")
   ((char-fx #\0) => "48~%")
   ((char-fx #\9) => "57~%")
   ((char-fx (fx-char 12)) => "12~%")
   ((fx-char (char-fx #\x)) => "#\\x~%"))

(deftest ("fixnump")
   ((fixnump 0) => "T~%")
   ((fixnump 1) => "T~%")
   ((fixnump -1) => "T~%")
   ((fixnump 37287) => "T~%")
   ((fixnump -23873) => "T~%")
   ((fixnump 536870911) => "T~%")
   ((fixnump -536870912) => "T~%")
   ((fixnump T) => "F~%")
   ((fixnump F) => "F~%")
   ((fixnump ()) => "F~%")
   ((fixnump #\Q) => "F~%")
   ((fixnump (fixnump 12)) => "F~%")
   ((fixnump (fixnump F)) => "F~%")
   ((fixnump (fixnump #\A)) => "F~%")
   ((fixnump (char-fx #\r)) => "T~%")
   ((fixnump (fx-char 12)) => "F~%"))

(deftest ("fxzerop")
   ((fxzerop 0) => "T~%")
   ((fxzerop 1) => "F~%")
   ((fxzerop -1) => "F~%")
   ((fxzerop #xC0) => "F~%"))

(deftest ("nilp")
   ((nilp ()) => "T~%")
   ((nilp F) => "F~%")
   ((nilp T) => "F~%")
   ((nilp (nilp ())) => "F~%")
   ((nilp #\a) => "F~%")
   ((nilp 0) => "F~%")
   ((nilp -10) => "F~%")
   ((nilp 10) => "F~%"))

(deftest ("boolp")
   ((boolp T) => "T~%")
   ((boolp F) => "T~%")
   ((boolp 0) => "F~%")
   ((boolp 1) => "F~%")
   ((boolp -1) => "F~%")
   ((boolp ()) => "F~%")
   ((boolp #\a) => "F~%")
   ((boolp (boolp 0)) => "T~%")
   ((boolp (fixnump (boolp 0))) => "T~%"))


(deftest ("charp")
   ((charp #\a) => "T~%")
   ((charp #\Z) => "T~%")
   ((charp #\newline) => "T~%")
   ((charp T) => "F~%")
   ((charp F) => "F~%")
   ((charp ()) => "F~%")
   ((charp (charp T)) => "F~%")
   ((charp 0) => "F~%")
   ((charp 23870) => "F~%")
   ((charp -23789) => "F~%"))

(deftest ("not")
  ((not T) => "F~%")
  ((not F) => "T~%")
  ((not 15) => "F~%")
  ((not ()) => "F~%")
  ((not #\A) => "F~%")
  ((not (not T)) => "T~%")
  ((not (not F)) => "F~%")
  ((not (not 15)) => "T~%")
  ((not (fixnump 15)) => "F~%")
  ((not (fixnump F)) => "T~%"))

(deftest ("fxlognot")
  ((fxlognot 0) => "-1~%")
  ((fxlognot -1) => "0~%")
  ((fxlognot 1) => "-2~%")
  ((fxlognot -2) => "1~%")
  ((fxlognot 536870911) => "-536870912~%")
  ((fxlognot -536870912) => "536870911~%")
  ((fxlognot (fxlognot 237463)) => "237463~%"))

;;; Step 4

(deftest ("if")
  ((if T 12 13) => "12~%")
  ((if F 12 13) => "13~%")
  ((if 0 12 13)  => "12~%")
  ((if () 43 ()) => "43~%")
  ((if T (if 12 13 4) 17) => "13~%")
  ((if F 12 (if F 13 4)) => "4~%")
  ((if #\X (if 1 2 3) (if 4 5 6)) => "2~%")
  ((if (not (boolp T)) 15 (boolp F)) => "T~%")
  ((if (if (charp #\a) (boolp #\b) (fixnump #\c)) 119 -23) => "-23~%")
  ((if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7) => "6~%") 
  ((if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7) => "7~%") 
  ((not (if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7)) => "F~%") 
  ((if (charp 12) 13 14) => "14~%")
  ((if (charp #\a) 13 14) => "13~%")
  ((fx1+ (if (fx1- 1) (fx1- 13) 14)) => "13~%"))

;;; Step 5

(deftest ("fx+")
  ((fx+ 1 2) => "3~%")
  ((fx+ 1 -2) => "-1~%")
  ((fx+ -1 2) => "1~%")
  ((fx+ -1 -2) => "-3~%")
  ((fx+ 536870911 -1) => "536870910~%")
  ((fx+ 536870910 1) => "536870911~%")
  ((fx+ -536870912 1) => "-536870911~%")
  ((fx+ -536870911 -1) => "-536870912~%")
  ((fx+ 536870911 -536870912) => "-1~%")
  ((fx+ 1 (fx+ 2 3)) => "6~%")
  ((fx+ 1 (fx+ 2 -3)) => "0~%")
  ((fx+ 1 (fx+ -2 3)) => "2~%")
  ((fx+ 1 (fx+ -2 -3)) => "-4~%")
  ((fx+ -1 (fx+ 2 3)) => "4~%")
  ((fx+ -1 (fx+ 2 -3)) => "-2~%")
  ((fx+ -1 (fx+ -2 3)) => "0~%")
  ((fx+ -1 (fx+ -2 -3)) => "-6~%")
  ((fx+ (fx+ 1 2) 3) => "6~%")
  ((fx+ (fx+ 1 2) -3) => "0~%")
  ((fx+ (fx+ 1 -2) 3) => "2~%")
  ((fx+ (fx+ 1 -2) -3) => "-4~%")
  ((fx+ (fx+ -1 2) 3) => "4~%")
  ((fx+ (fx+ -1 2) -3) => "-2~%")
  ((fx+ (fx+ -1 -2) 3) => "0~%")
  ((fx+ (fx+ -1 -2) -3) => "-6~%")
  ((fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ 1 2) 3) 4) 5) 6) 7) 8) 9) => "45~%")
  ((fx+ 1 (fx+ 2 (fx+ 3 (fx+ 4 (fx+ 5 (fx+ 6 (fx+ 7 (fx+ 8 9)))))))) => "45~%"))

(deftest ("fx-")
  ((fx- 1 2) => "-1~%")
  ((fx- 1 -2) => "3~%")
  ((fx- -1 2) => "-3~%")
  ((fx- -1 -2) => "1~%")
  ((fx- 536870910 -1) => "536870911~%")
  ((fx- 536870911 1) => "536870910~%")
  ((fx- -536870911 1) => "-536870912~%")
  ((fx- -536870912 -1) => "-536870911~%")
  ((fx- 1 536870911) => "-536870910~%")
  ((fx- -1 536870911) => "-536870912~%")
  ((fx- 1 -536870910) => "536870911~%")
  ((fx- -1 -536870912) => "536870911~%")
  ((fx- 536870911 536870911) => "0~%")
  ;((fx- 536870911 -536870912) => "-1~%")
  ((fx- -536870911 -536870912) => "1~%")
  ((fx- 1 (fx- 2 3)) => "2~%")
  ((fx- 1 (fx- 2 -3)) => "-4~%")
  ((fx- 1 (fx- -2 3)) => "6~%")
  ((fx- 1 (fx- -2 -3)) => "0~%")
  ((fx- -1 (fx- 2 3)) => "0~%")
  ((fx- -1 (fx- 2 -3)) => "-6~%")
  ((fx- -1 (fx- -2 3)) => "4~%")
  ((fx- -1 (fx- -2 -3)) => "-2~%")
  ((fx- 0 (fx- -2 -3)) => "-1~%")
  ((fx- (fx- 1 2) 3) => "-4~%")
  ((fx- (fx- 1 2) -3) => "2~%")
  ((fx- (fx- 1 -2) 3) => "0~%")
  ((fx- (fx- 1 -2) -3) => "6~%")
  ((fx- (fx- -1 2) 3) => "-6~%")
  ((fx- (fx- -1 2) -3) => "0~%")
  ((fx- (fx- -1 -2) 3) => "-2~%")
  ((fx- (fx- -1 -2) -3) => "4~%")
  ((fx- (fx- (fx- (fx- (fx- (fx- (fx- (fx- 1 2) 3) 4) 5) 6) 7) 8) 9) => "-43~%")
  ((fx- 1 (fx- 2 (fx- 3 (fx- 4 (fx- 5 (fx- 6 (fx- 7 (fx- 8 9)))))))) => "5~%"))

(deftest ("fx*")
  ((fx* 2 3) => "6~%")
  ((fx* 2 -3) => "-6~%")
  ((fx* -2 3) => "-6~%")
  ((fx* -2 -3) => "6~%")
  ((fx* 536870911 1) => "536870911~%")
  ((fx* 536870911 -1) => "-536870911~%")
  ((fx* -536870912 1) => "-536870912~%")
  ((fx* -536870911 -1) => "536870911~%")
  ((fx* 2 (fx* 3 4)) => "24~%")
  ((fx* (fx* 2 3) 4) => "24~%")
  ((fx* (fx* (fx* (fx* (fx* 2 3) 4) 5) 6) 7) => "5040~%")
  ((fx* 2 (fx* 3 (fx* 4 (fx* 5 (fx* 6 7))))) => "5040~%"))

(deftest ("fxlogand and fxlogor")
  ((fxlogor 3 16) => "19~%")
  ((fxlogor 3 5)  => "7~%")
  ((fxlogor 3 7)  => "7~%")
  ((fxlognot (fxlogor (fxlognot 7) 1)) => "6~%")
  ((fxlognot (fxlogor 1 (fxlognot 7))) => "6~%")
  ((fxlogand 3 7) => "3~%")
  ((fxlogand 3 5) => "1~%")
  ((fxlogand 2346 (fxlognot 2346)) => "0~%")
  ((fxlogand (fxlognot 2346) 2346) => "0~%")
  ((fxlogand 2376 2376) => "2376~%"))

(deftest ("fx=")
  ((fx= 12 13) => "F~%")
  ((fx= 12 12) => "T~%")
  ((fx= #xF00 #xE00) => "F~%")
  ((fx= 16 (fx+ 13 3)) => "T~%")
  ((fx= 16 (fx+ 13 13)) => "F~%")
  ((fx= (fx+ 13 3) 16) => "T~%")
  ((fx= (fx+ 13 13) 16) => "F~%"))

(deftest ("fx<")
  ((fx< 12 13) => "T~%")
  ((fx< 12 12) => "F~%")
  ((fx< 13 12) => "F~%")
  ((fx< #xE01 #xF00) => "T~%")
  ((fx< 16 (fx+ 13 1)) => "F~%")
  ((fx< 16 (fx+ 13 3)) => "F~%")
  ((fx< 16 (fx+ 13 13)) => "T~%")
  ((fx< (fx+ 13 1) 16) => "T~%")
  ((fx< (fx+ 13 3) 16) => "F~%")
  ((fx< (fx+ 13 13) 16) => "F~%"))

(deftest ("fx<=")
  ((fx<= 12 13) => "T~%")
  ((fx<= 12 12) => "T~%")
  ((fx<= 13 12) => "F~%")
  ((fx<= #xE01 #xF00) = "T~%")
  ((fx<= 16 (fx+ 13 1)) => "F~%")
  ((fx<= 16 (fx+ 13 3)) => "T~%")
  ((fx<= 16 (fx+ 13 13)) => "T~%")
  ((fx<= (fx+ 13 1) 16) => "T~%")
  ((fx<= (fx+ 13 3) 16) => "T~%")
  ((fx<= (fx+ 13 13) 16) => "F~%"))

(deftest ("fx>")
  ((fx> 12 13) => "F~%")
  ((fx> 12 12) => "F~%")
  ((fx> 13 12) => "T~%")
  ((fx> #xF00 #xE01) => "T~%")
  ((fx> 16 (fx+ 13 1)) => "T~%")
  ((fx> 16 (fx+ 13 3)) => "F~%")
  ((fx> 16 (fx+ 13 13)) => "F~%")
  ((fx> (fx+ 13 1) 16) => "F~%")
  ((fx> (fx+ 13 3) 16) => "F~%")
  ((fx> (fx+ 13 13) 16) => "T~%"))

(deftest ("fx>=")
  ((fx>= 12 13) => "F~%")
  ((fx>= 12 12) => "T~%")
  ((fx>= 13 12) => "T~%")
  ((fx>= #xF00 #xE01) => "T~%")
  ((fx>= 16 (fx+ 13 1)) => "T~%")
  ((fx>= 16 (fx+ 13 3)) => "T~%")
  ((fx>= 16 (fx+ 13 13)) => "F~%")
  ((fx>= (fx+ 13 1) 16) => "F~%")
  ((fx>= (fx+ 13 3) 16) => "T~%")
  ((fx>= (fx+ 13 13) 16) => "T~%"))

(deftest ("if and binary")
  ((if (fx= 12 13) 12 13) => "13~%")
  ((if (fx= 12 12) 13 14) => "13~%")
  ((if (fx< 12 13) 12 13) => "12~%")
  ((if (fx< 12 12) 13 14) => "14~%")
  ((if (fx< 13 12) 13 14) => "14~%")
  ((if (fx<= 12 13) 12 13) => "12~%")
  ((if (fx<= 12 12) 12 13) => "12~%")
  ((if (fx<= 13 12) 13 14) => "14~%")
  ((if (fx> 12 13) 12 13) => "13~%")
  ((if (fx> 12 12) 12 13) => "13~%")
  ((if (fx> 13 12) 13 14) => "13~%")
  ((if (fx>= 12 13) 12 13) => "13~%")
  ((if (fx>= 12 12) 12 13) => "12~%")
  ((if (fx>= 13 12) 13 14) => "13~%"))

(deftest ("binary primitives")
  ((fxlognot -7) => "6~%")
  ((fxlognot (fxlogor (fxlognot 7) 1)) => "6~%")
  ((fxlognot (fxlogor (fxlognot 7) (fxlognot 2))) => "2~%")
  ((fxlogand (fxlognot (fxlognot 12)) (fxlognot (fxlognot 12))) => "12~%")
  ((fx+ (fx+ 1 2) (fx+ 3 4)) => "10~%")
  ((fx+ (fx+ 1 2) (fx+ 3 -4)) => "2~%")
  ((fx+ (fx+ 1 2) (fx+ -3 4)) => "4~%")
  ((fx+ (fx+ 1 2) (fx+ -3 -4)) => "-4~%")
  ((fx+ (fx+ 1 -2) (fx+ 3 4)) => "6~%")
  ((fx+ (fx+ 1 -2) (fx+ 3 -4)) => "-2~%")
  ((fx+ (fx+ 1 -2) (fx+ -3 4)) => "0~%")
  ((fx+ (fx+ 1 -2) (fx+ -3 -4)) => "-8~%")
  ((fx+ (fx+ -1 2) (fx+ 3 4)) => "8~%")
  ((fx+ (fx+ -1 2) (fx+ 3 -4)) => "0~%")
  ((fx+ (fx+ -1 2) (fx+ -3 4)) => "2~%")
  ((fx+ (fx+ -1 2) (fx+ -3 -4)) => "-6~%")
  ((fx+ (fx+ -1 -2) (fx+ 3 4)) => "4~%")
  ((fx+ (fx+ -1 -2) (fx+ 3 -4)) => "-4~%")
  ((fx+ (fx+ -1 -2) (fx+ -3 4)) => "-2~%")
  ((fx+ (fx+ -1 -2) (fx+ -3 -4)) => "-10~%")
  ((fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ 1 2) 3) 4) 5) 6) 7) 8) 9) => "45~%")
  ((fx+ 1 (fx+ 2 (fx+ 3 (fx+ 4 (fx+ 5 (fx+ 6 (fx+ 7 (fx+ 8 9)))))))) => "45~%")
  ((fx+ (fx+ (fx+ (fx+ 1 2) (fx+ 3 4)) (fx+ (fx+ 5 6) (fx+ 7 8)))
        (fx+ (fx+ (fx+ 9 10) (fx+ 11 12)) (fx+ (fx+ 13 14) (fx+ 15 16)))) 
   => "136~%")
  ((fx- (fx- 1 2) (fx- 3 4)) => "0~%")
  ((fx- (fx- 1 2) (fx- 3 -4)) => "-8~%")
  ((fx- (fx- 1 2) (fx- -3 4)) => "6~%")
  ((fx- (fx- 1 2) (fx- -3 -4)) => "-2~%")
  ((fx- (fx- 1 -2) (fx- 3 4)) => "4~%")
  ((fx- (fx- 1 -2) (fx- 3 -4)) => "-4~%")
  ((fx- (fx- 1 -2) (fx- -3 4)) => "10~%")
  ((fx- (fx- 1 -2) (fx- -3 -4)) => "2~%")
  ((fx- (fx- -1 2) (fx- 3 4)) => "-2~%")
  ((fx- (fx- -1 2) (fx- 3 -4)) => "-10~%")
  ((fx- (fx- -1 2) (fx- -3 4)) => "4~%")
  ((fx- (fx- -1 2) (fx- -3 -4)) => "-4~%")
  ((fx- (fx- -1 -2) (fx- 3 4)) => "2~%")
  ((fx- (fx- -1 -2) (fx- 3 -4)) => "-6~%")
  ((fx- (fx- -1 -2) (fx- -3 4)) => "8~%")
  ((fx- (fx- -1 -2) (fx- -3 -4)) => "0~%")
  ((fx- (fx- (fx- (fx- (fx- (fx- (fx- (fx- 1 2) 3) 4) 5) 6) 7) 8) 9) => "-43~%")
  ((fx- 1 (fx- 2 (fx- 3 (fx- 4 (fx- 5 (fx- 6 (fx- 7 (fx- 8 9)))))))) => "5~%")
  ((fx- (fx- (fx- (fx- 1 2) (fx- 3 4)) (fx- (fx- 5 6) (fx- 7 8)))
        (fx- (fx- (fx- 9 10) (fx- 11 12)) (fx- (fx- 13 14) (fx- 15 16)))) 
   => "0~%")
  ((fx* (fx* (fx* (fx* 2 3) (fx* 4 5)) (fx* (fx* 6 7) (fx* 8 9)))
        (fx* (fx* (fx* 2 3) (fx* 2 3)) (fx* (fx* 2 3) (fx* 2 3)))) 
   => "470292480~%")
  ((fxlognot (fxlogor (fxlognot 7) 1)) => "6~%")
  ((fxlognot (fxlogor (fxlognot 7) (fxlognot 2))) => "2~%")
  ((fxlogand (fxlognot (fxlognot 12)) (fxlognot (fxlognot 12))) => "12~%")
  ((fx= (fx+ 13 3) (fx+ 10 6)) => "T~%")
  ((fx= (fx+ 13 0) (fx+ 10 6)) => "F~%")
  ((fx= (fx+ 12 1) (fx+ -12 -1)) => "F~%")
  ((fx< (fx+ 10 6) (fx+ 13 1)) => "F~%")
  ((fx< (fx+ 10 6) (fx+ 13 3)) => "F~%")
  ((fx< (fx+ 10 6) (fx+ 13 31)) => "T~%")
  ((fx< (fx+ 12 1) (fx+ -12 -1)) => "F~%")
  ((fx< (fx+ -12 -1) (fx+ 12 1)) => "T~%")
  ((fx<= (fx+ 10 6) (fx+ 13 1)) => "F~%")
  ((fx<= (fx+ 10 6) (fx+ 13 3)) => "T~%")
  ((fx<= (fx+ 10 6) (fx+ 13 31)) => "T~%")
  ((fx<= (fx+ 12 1) (fx+ -12 -1)) => "F~%")
  ((fx<= (fx+ -12 -1) (fx+ 12 1)) => "T~%")
  ((fx> (fx+ 10 6) (fx+ 13 1)) => "T~%")
  ((fx> (fx+ 10 6) (fx+ 13 3)) => "F~%")
  ((fx> (fx+ 10 6) (fx+ 13 31)) => "F~%")
  ((fx> (fx+ 12 1) (fx+ -12 -1)) => "T~%")
  ((fx> (fx+ -12 -1) (fx+ 12 1)) => "F~%")
  ((fx>= (fx+ 10 6) (fx+ 13 1)) => "T~%")
  ((fx>= (fx+ 10 6) (fx+ 13 3)) => "T~%")
  ((fx>= (fx+ 10 6) (fx+ 13 31)) => "F~%")
  ((fx>= (fx+ 12 1) (fx+ -12 -1)) => "T~%")
  ((fx>= (fx+ -12 -1) (fx+ 12 1)) => "F~%"))

;;; Step 6
(deftest ("let")
  ((let ((x 5)) x) => "5~%")
  ((let ((x (fx+ 1 2))) x) => "3~%")
  ((let ((x (fx+ 1 2))) 
     (let ((y (fx+ 3 4)))
       (fx+ x y))) 
   => "10~%")
  ((let ((x (fx+ 1 2))) 
     (let ((y (fx+ 3 4)))
       (fx- y x)))
   => "4~%")
  ((let ((x (fx+ 1 2))
         (y (fx+ 3 4)))
     (fx- y x))
   => "4~%")
  ((let ((x (let ((y (fx+ 1 2))) (fx* y y))))
     (fx+ x x))
   => "18~%")
  ((let ((x (fx+ 1 2)))
     (let ((x (fx+ 3 4)))
       x)) 
   => "7~%")
  ((let ((x (fx+ 1 2)))
     (let ((x (fx+ x 4)))
       x)) 
   => "7~%")
  ((let ((l (let ((l (let ((l (let ((l (fx+ 1 2))) l))) l))) l))) l)
   => "3~%")
  ((let ((x 12))
     (let ((x (fx+ x x)))
       (let ((x (fx+ x x)))
         (let ((x (fx+ x x)))
           (fx+ x x)))))
   => "192~%"))

;;; Step 6 opt
(deftest ("let*")
  ((let* ((x 5)) x) => "5~%")
  ((let* ((x (fx+ 1 2))) x) => "3~%")
  ((let* ((x (fx+ 1 2)) 
               (y (fx+ x 4)))
     (fx+ x y)) 
   => "10~%")
  ((let* ((x (fx+ 1 2))
              (y (fx+ x 4)))
     (fx- y x))
   => "4~%")
  ((let* ((x 12)
              (x (fx+ x x))
              (x (fx+ x x))
              (x (fx+ x x)))
     (fx+ x x))
   => "192~%"))

;;; Step 7

(deftest ("procedures")
  ((letrec () 12) => "12~%")
  ((letrec () (let ((x 5)) (fx+ x x))) => "10~%")
  ((letrec ((l (lambda () 5))) 7) => "7~%")
  ((letrec ((l (lambda () 5))) (let ((x 12)) x)) => "12~%")
  ((letrec ((l (lambda () 5))) (l)) => "5~%")
  ((letrec ((l (lambda () 5))) (let ((x (l))) x)) => "5~%")
  ((letrec ((l (lambda () 5))) (fx+ (l) 6)) => "11~%")
  ((letrec ((l (lambda () 5))) (fx- 20 (l))) => "15~%")
  ((letrec ((l (lambda () 5))) (fx+ (l) (l))) => "10~%")
  ((letrec ((l (lambda () (fx+ 5 7)))
            (g (lambda () 13))) 
    (fx+ (l) (g))) => "25~%")
  ((letrec ((l (lambda (x) (fx+ x 12)))) (l 13)) => "25~%")
  ((letrec ((l (lambda (x) (fx+ x 12)))) (l (l 10))) => "34~%")
  ((letrec ((l (lambda (x) (fx+ x 12)))) (l (l (l 0)))) => "36~%")
  ((letrec ((l (lambda (x y) (fx+ x y))) 
            (g (lambda (x) (fx+ x 12))))
    (l 16 (l (g 0) (fx+ 1 (g 0))))) => "41~%")
  ((letrec ((l (lambda (x) (g x x)))
            (g (lambda (x y) (fx+ x y))))
     (l 12)) => "24~%")
  ((letrec ((l (lambda (x) 
                 (if (fxzerop x)
                     1
                     (fx* x (l (fx1- x)))))))
      (l 5)) => "120~%")
  ((letrec ((e (lambda (x) (if (fxzerop x) T (o (fx1- x)))))
            (o (lambda (x) (if (fxzerop x) F (e (fx1- x))))))
     (e 25)) => "F~%"))

(deftest ("deeply nested procedures (notail safe)")
  ((letrec ((sum (lambda (n ac)
                   (if (fxzerop n)
                        ac
                        (app sum (fx1- n) (fx+ n ac))))))
    (app sum 1000 0)) => "500500~%")
  ((letrec ((e (lambda (x) (if (fxzerop x) T (app o (fx1- x)))))
            (o (lambda (x) (if (fxzerop x) F (app e (fx1- x))))))
     (app e 5000)) => "T~%"))

(deftest ("deeply nested procedures (notail unsafe)")
  ((letrec ((sum (lambda (n ac)
                   (if (fxzerop n)
                        ac
                        (app sum (fx1- n) (fx+ n ac))))))
    (app sum 10000 0)) => "50005000~%")
  ((letrec ((e (lambda (x) (if (fxzerop x) T (app o (fx1- x)))))
            (o (lambda (x) (if (fxzerop x) F (app e (fx1- x))))))
     (app e 5000000)) => "T~%"))

;;; step 8

(deftest ("cons")
  ((fx1+ 0) => "1~%")
  ((consp (cons 1 2)) => "T~%")
  ((consp 12) => "F~%")
  ((consp T) => "F~%")
  ((consp F) => "F~%")
  ((consp ()) => "F~%")
  ((fixnump (cons 12 43)) => "F~%")
  ((boolp (cons 12 43)) => "F~%")
  ((nilp (cons 12 43)) => "F~%")
  ((not (cons 12 43)) => "F~%")
  ((if (cons 12 43) 32 43) => "32~%")
  ((car (cons 1 23)) => "1~%")
  ((cdr (cons 43 123)) => "123~%")
  ((car (car (cons (cons 12 3) (cons T F)))) => "12~%")
  ((cdr (car (cons (cons 12 3) (cons T F)))) => "3~%")
  ((car (cdr (cons (cons 12 3) (cons T F)))) => "T~%")
  ((cdr (cdr (cons (cons 12 3) (cons T F)))) => "F~%")
  ((let ((x (let ((y (fx+ 1 2))) (fx* y y))))
     (cons x (fx+ x x)))
   => "(9 . 18)~%")
  ((let ((t0 (cons 1 2)) (t1 (cons 3 4)))
     (let ((a0 (car t0)) (a1 (car t1)) (d0 (cdr t0)) (d1 (cdr t1)))
       (let ((t0 (cons a0 d1)) (t1 (cons a1 d0)))
         (cons t0 t1))))
   => "((1 . 4) 3 . 2)~%")
  ((let ((u (cons 1 2)))
     (let ((u u))
       (let ((u u))
         (let ((u u))
           u))))
   => "(1 . 2)~%")
  ((let ((u (let ((u (let ((u (let ((u (cons 1 2))) u))) u))) u))) u)
   => "(1 . 2)~%")
  ((let ((x ()))
     (let ((x (cons x x)))
       (let ((x (cons x x)))
         (let ((x (cons x x)))
           (cons x x)))))
   => "((((NIL) NIL) (NIL) NIL) ((NIL) NIL) (NIL) NIL)~%")
  ((cons (let ((x T)) (let ((y (cons x x))) (cons x y)))
         (cons (let ((x F)) (let ((y (cons x x))) (cons y x))) 
               ())) 
   => "((T T . T) ((F . F) . F))~%"))

(deftest ("progn/implicit-progn")
 ((progn 12) => "12~%")
 ((progn 13 122) => "122~%")
 ((progn 123 2343 T) => "T~%")
 ((let ((l (progn 12 (cons 1 2)))) (progn l l)) => "(1 . 2)~%")
 ((let ((l (progn 13 (cons 1 2))))
    (cons 1 l)
    l) => "(1 . 2)~%")
 ((let ((l (cons 1 2)))
    (if (consp l) 
        (progn l)
        12)) => "(1 . 2)~%")
)

(deftest ("strings")
  ((stringp (make-string 0)) => "T~%")
  ((make-string 0) => "\"\"~%")
  ((let ((s (make-string 1))) 
     (string-set s 0 #\a)
     (string-ref s 0)) => "#\\a~%")
  
  ((let ((s (make-string 2))) 
     (string-set s 0 #\a)
     (string-set s 1 #\b)
     (cons (string-ref s 0) (string-ref s 1))) => "(#\\a . #\\b)~%")
  ((let ((i 0))
    (let ((s (make-string 1))) 
     (string-set s i #\a)
     (string-ref s i))) => "#\\a~%")
  ((let ((i 0) (j 1))
    (let ((s (make-string 2))) 
     (string-set s i #\a)
     (string-set s j #\b)
     (cons (string-ref s i) (string-ref s j)))) => "(#\\a . #\\b)~%")
  ((let ((i 0) (c #\a))
    (let ((s (make-string 1))) 
     (string-set s i c)
     (string-ref s i))) => "#\\a~%")
  ((string-length (make-string 12)) => "12~%")
  ;((stringp (make-vector 12)) => "F~%")
  ((stringp (cons 1 2)) => "F~%")
  ((stringp 1287) => "F~%")
  ((stringp ()) => "F~%")
  ((stringp T) => "F~%")
  ((stringp F) => "F~%")
  ((consp (make-string 12)) => "F~%")
  ((nilp (make-string 12)) => "F~%")
  ((boolp (make-string 12)) => "F~%")
  ;((vector? (make-string 12)) => "F~%")
  ((make-string 0) => "\"\"~%")
  ((let ((v (make-string 2)))
     (string-set v 0 #\t)
     (string-set v 1 #\f)
     v) => "\"tf\"~%")
  ((let ((v (make-string 2)))
     (string-set v 0 #\x)
     (string-set v 1 #\x)
     (char= (string-ref v 0) (string-ref v 1))) => "T~%")
  ((let ((v0 (make-string 3)))
     (let ((v1 (make-string 3)))
       (string-set v0 0 #\a)
       (string-set v0 1 #\b)
       (string-set v0 2 #\c)
       (string-set v1 0 #\d)
       (string-set v1 1 #\e)
       (string-set v1 2 #\f)
       (cons v0 v1))) => "(\"abc\" . \"def\")~%")
  ((let ((n 2))
    (let ((v0 (make-string n)))
     (let ((v1 (make-string n)))
       (string-set v0 0 #\a)
       (string-set v0 1 #\b)
       (string-set v1 0 #\c)
       (string-set v1 1 #\d)
       (cons v0 v1)))) => "(\"ab\" . \"cd\")~%")
  ((let ((n 3))
    (let ((v0 (make-string n)))
     (let ((v1 (make-string (string-length v0))))
       (string-set v0 (fx- (string-length v0) 3) #\a)
       (string-set v0 (fx- (string-length v1) 2) #\b)
       (string-set v0 (fx- (string-length v0) 1) #\c)
       (string-set v1 (fx- (string-length v1) 3) #\Z)
       (string-set v1 (fx- (string-length v0) 2) #\Y)
       (string-set v1 (fx- (string-length v1) 1) #\X)
       (cons v0 v1)))) =>  "(\"abc\" . \"ZYX\")~%")
  ((let ((n 1))
     (string-set (make-string n) (fx1- n) (fx-char 34))
     n) => "1~%")
  ((let ((n 1))
     (let ((v (make-string 1)))
       (string-set v (fx1- n) (fx-char n))
       (char-fx (string-ref v (fx1- n))))) => "1~%")
 ((let ((v0 (make-string 1)))
    (string-set v0 0 #\a)
    (let ((v1 (make-string 1)))
      (string-set v1 0 #\A)
      (string-set (if (stringp v0) v0 v1) 
           (fx1- (string-length (if (stringp v0) v0 v1)))
           (fx-char
             (fx1+ 
                (char-fx
                  (string-ref
                     (if (stringp v0) v0 v1)
                     (fx1- (string-length (if (stringp v0) v0 v1))))))))
      (cons v0 v1))) => "(\"b\" . \"A\")~%")
 ((let ((s (make-string 1)))
     (string-set s 0 #\")
     s) => "\"\\\"\"~%")
 ((let ((s (make-string 1)))
     (string-set s 0 #\\)
     s) => "\"\\\\\"~%"))