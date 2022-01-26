(defpackage :software-evolution-library/utility/include
  (:use :gt/full)
  (:use
    :software-evolution-library
    :software-evolution-library/software/parseable
    :software-evolution-library/software/tree-sitter)
  (:import-from :serapeum :~>>)
  (:export :extract-llvm-synopses))
(in-package :software-evolution-library/utility/include)

(def +llvm-header+ "// -*- C++ -*-
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//")

(defun regenerate-llvm-synopses (source-dir dest-dir)
  (declare (optimize debug))
  (assert (directory-exists-p source-dir))
  (ensure-directories-exist dest-dir)
  (setf source-dir (path-join source-dir "libcxx/include/"))
  (assert (directory-exists-p source-dir))
  (let ((files
         (nest
          (remove-if (op (string^= "__" (pathname-name _))))
          (remove-if #'pathname-type)
          (filter #'file-pathname-p)
          (list-directory source-dir))))
    (iter (for file in files)
          (when-let (synopsis-code (extract-include-synopsis file))
            (collect file)
            (let ((out (path-join dest-dir (pathname-name file))))
              (when (file-exists-p out)
                ;; If we're overwriting a file, make sure it has the
                ;; LLVM project header.
                (assert (search +llvm-header+ (read-file-into-string out))))
              (with-output-to-file (stream out :if-exists :supersede)
                (format stream "~a~3&~a" +llvm-header+
                        synopsis-code)))))))

(defun extract-include-synopsis (file)
  (let ((file-string (read-file-into-string file)))
    ;; As of 2022-01-26, doesn't have a synopsis.
    (when (equal (pathname-name file) "execution")
      (return-from extract-include-synopsis nil))
    (when (equal (pathname-name file) "format")
      (setf file-string
            (string-replace "/*" file-string "/* synopsis ")))
    (let ((synopsis (find "synopsis"
                          (split "/\\*|\\*/" file-string)
                          :test #'search)))
      (apply #'string+
             (drop-while (op (or (blankp _1)
                                 (search "synopsis" _1)))
                         (lines synopsis :keep-eols t))))))
