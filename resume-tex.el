;;; resume-tex.el --- provides resume latex generation -*- lexical-binding: t; -*-
;
;; Copyright (C) 2025 Andrew Peck

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
;;
;;; Code:

(defun resume--latex-project (project)
  "")

(defun resume--latex-skill (skill)
  "")

(defun resume--latex-education (education)
  "")

(defun resume--latex-job (job)
  (format "\\job{%s}{%s}{%s}{%s}"
          (plist-get job :company)
          (plist-get job :location)
          (plist-get job :title)
          (plist-get job :dates)))

(defun resume--latex-contact-info (contact-info)
  (format "\\name{%s}{%s}{%s}{%s}{%s}{%s}{%s}"
          (plist-get contact-info :name)
          (plist-get contact-info :title)
          (plist-get contact-info :address)
          (plist-get contact-info :phone)
          (plist-get contact-info :email)
          (plist-get contact-info :github)
          (plist-get contact-info :linkedin)))

(defun resume-make-latex (contact-info jobs projects skills project-intro educations)
  (f-write-text (string-join
                 `(,(resume--latex-header)
                   ,(resume--latex-contact-info contact-info)
                   "\\vspace{-10pt}"

                   "\\heading{Skills}"
                   ,(string-join (mapcar #'resume--latex-skill skills) "\n")

                   "\\heading{Job History}"
                   ,(string-join (mapcar #'resume--latex-job jobs) "\n")

                   "\\heading{Education}"
                   ,(string-join (mapcar #'resume--latex-education educations) "\n")

                   "\\heading{Selected Projects}"
                   ,(string-join (mapcar #'resume--latex-project projects) "\n")
                   "\\end{document}")
                 "\n") 'utf-8 "resume-gen.tex"))

(provide 'resume-tex)
;;; resume-tex.el ends here
