;;; wsl-path.el -- Teach EMACS about windows subsystem for linux styles paths

;; Copyright (C) 2009 Victor Ren
;; Copyright (C) 2019-08-29 Zach Kost-Smith
;;               2023-11-02 Todd Goldfinger
;; Author: Victor Ren <victorhge@gmail.com>
;; Modified for WSL by: Zach Kost-Smith <zachkostsmith@gmail.com>
;; Keywords: windows, WSL, mount, path

;; This file is *NOT* (yet?) part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package lets you use windows-style filenames like "c:/path/file" or
;; "c:\path\file" in Emacs when running in the Windows Subsystem for Linux.

;;; Installation:

;; Put in your .emacs or site-start.el file the following lines:
;;   (require 'wsl-path)
;;   (wsl-path-activate)

;;; Compatibility

;; How it works: Push some functions onto file-name-handler-alist.  which detect
;; filenames expressed in Windows style, and translate those names into the WSL
;; equivalent.

;;; Code:

(require 'cl-lib)
(defconst wsl-path-version "0.2")
(defconst wsl-path-style1-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\|[.]\\)\\\\")
(defconst wsl-path-style2-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)/")
(defvar wsl-path-activated nil)

(defgroup wsl-path nil
  "Proper handling of windows filenames."
  :prefix "wsl-path-"
  :group 'files)

(defvar wslpath-exec "wslpath"
  "The wslpath executable location.")

(defun wsl-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))


(defun wsl-path-convert-to-linux (name)
  "Convert file NAME to WSL style (`x:/' to `/mnt/x/')."
    (if (or (string-match wsl-path-style1-regexp name)
            (string-match wsl-path-style2-regexp name))
        (unwind-protect
            ;; inhibit only works for one operation, but there are multiple that happen in call-process
            (with-temp-buffer
              (wsl-path-deactivate)
              (call-process wslpath-exec nil t nil "-u" name)
              (s-trim (buffer-string)))
          (wsl-path-activate))
      name))

(defun wsl-path-convert-to-win (name)
  "Convert file NAME to WSL style (`/mnt/x/' to `x:/')."
  (with-temp-buffer
    (call-process wslpath-exec nil t nil "-m" name)
    (s-trim (buffer-string))))

(defun wsl-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on WSL NAME with ARGS.
Map Windows sytle name to the WSL-style \"/[A-Za-z]/\" and call
OPERATION with the mapped filename\(s). NAME must have the format looks like
\"^/[A-Za-z]:/\" or \"^[A-Za-z]:\\\"  here. Note that at least the first
element of ARGS could be a filename too \(then it must have the same syntax
like NAME!) which must be converted \(e.g. `expand-file-name' can be called
with two filenames)."
  (wsl-path-run-real-handler
   operation
   (cons (wsl-path-convert-to-linux name)
		 (if (stringp (car args))
			 (cons (wsl-path-convert-to-linux (car args))
				   (cdr args))
		   args))))

(defun wsl-path-activate ()
  "Activate wsl-path-style-handling."
  (interactive)

  (unless wsl-path-activated
    (add-to-list 'file-name-handler-alist
                 (cons wsl-path-style1-regexp
                       'wsl-path-map-drive-hook-function))
    (add-to-list 'file-name-handler-alist
                 (cons wsl-path-style2-regexp
                       'wsl-path-map-drive-hook-function))
    (setq wsl-path-activated t)))

(defun wsl-path-deactivate ()
  "Deactivate windows-style-path handling."
  (interactive)
  (unless (not wsl-path-activated)

    (setq file-name-handler-alist
          (delete (assoc wsl-path-style1-regexp file-name-handler-alist)
                  file-name-handler-alist))
    (setq file-name-handler-alist
          (delete (assoc wsl-path-style2-regexp file-name-handler-alist)
                  file-name-handler-alist))

    (setq wsl-path-activated nil)))

(provide 'wsl-path)

;;; wsl-path.el ends here
