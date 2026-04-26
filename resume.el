;;; resume.el --- Package to generate a resume -*- lexical-binding: t; -*-
;
;; Copyright (C) 2025-2026 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/resume.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dash "2.0") (f "0.20") (s "1.12"))
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
;;;
;; This package provides some simple infrastructure for generating LaTex and
;; HTML resumes from an elisp datastructure. Quite opinionated and special
;; purpose (for me) but if you aren't too picky or share my opinions or are
;; willing to hack a bit it should work for you as well.
;;
;; What problem does this solve? I wanted both HTML and PDF versions of the same
;; resume.
;;
;; And of course the best way to do this is in emacs...
;;
;; For real examples see:
;;
;; - https://andrewpeck.xyz/resume.html
;; - https://andrewpeck.xyz/resume.pdf
;;
;;; Code:

(require 'f)
(require 'resume-tex)
(require 'resume-html)

(defgroup resume nil
  "Functions to generate resumes from elisp data structures."
  :group 'resume
  :link '(url-link "https://github.com/andrewpeck/resume.el")
  :prefix "resume")

;;;###autoload
(defun resume-make (contact-info jobs projects skills project-intro educations)
  "Generate HTML and Latex resume documents.

Generate HTML and LaTeX resume documents from provided details.

Takes CONTACT-INFO, JOBS, PROJECTS, SKILLS, PROJECT-INTRO, and EDUCATIONS
as arguments to create resumes in both formats."
  ;; TODO: copy css file over also
  (resume-make-latex contact-info jobs projects skills project-intro educations)
  (resume-make-html contact-info jobs projects skills project-intro educations))

(provide 'resume)
;;; resume.el ends here
