#!/bin/sh

# Copyright (c) 2008, Piotr Esden-Tempski <piotr at esden.net>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are 
# met:
# 
# * Redistributions of source code must retain the above copyright notice, 
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright 
#   notice, this list of conditions and the following disclaimer in the 
#   documentation and/or other materials provided with the distribution.
# * The names of its contributors may not be used to endorse or promote 
#   products derived from this software without specific prior written 
#   permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
