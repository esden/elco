#!/bin/sh

ELCO_DRIVER_FILE="elco/driver.lisp"

echo '(in-package #:elco)' > $ELCO_DRIVER_FILE
echo '(defun create-driver-object ()
 (let ((tmp (make-temp-file-name "c"))
       (driver-object (make-temp-file-name "o")))
	(with-open-file (driver-stream tmp :direction :output :if-exists :supersede)
	 (princ "' >> $ELCO_DRIVER_FILE

cat elco/character-table.h elco/driver.c | sed 's,\\,\\\\,g' | sed 's,",\\",g' >> $ELCO_DRIVER_FILE

echo '" driver-stream))
   (sb-ext:run-program "/usr/bin/gcc"
                       `("-O3"
			 "-Wall"
			 "-c"
			 ,(namestring tmp)
			 "-o"
			 ,(namestring driver-object)))
   (sb-ext:run-program "/bin/rm" `("-f" ,(namestring tmp)))
   driver-object))' >> $ELCO_DRIVER_FILE

echo '(defvar *driver-object* (create-driver-object))' >> $ELCO_DRIVER_FILE
