;;; resume.el --- package to generate a resume -*- lexical-binding: t; -*-
;
;; Copyright (C) 2025 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/resume.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;;; Code:

(require 'f)
(require 'resume-tex)
(require 'resume-html)

(defun resume-make (contact-info jobs projects skills project-intro educations)
  "Generate HTML and Latex resume documents."
  (resume-make-latex contact-info jobs projects skills project-intro schools)
  (resume-make-html contact-info jobs projects skills project-intro schools)
  ;; TODO: copy css file over also
  )

(provide 'resume)
;;; resume.el ends here
