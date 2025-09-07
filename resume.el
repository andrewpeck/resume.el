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

(defun resume--html-header ()
  ""
  "<html>
<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"resume.css\">
</head>
</body>
<div id=\"resume\">
")

(defun resume--html-trailer ()
  ""
  "</div></body></html>")

(defun resume--html-project (project)
  "Format a single PROJECT into an html div."
  (concat
   "<div>"
   (format "<div class=\"projectName\">%s</div>" (plist-get project :project))
   "<ul>"
   (apply #'concat
    (mapcar (lambda (x) (format "<li>%s</li>" x))
            (plist-get project :tasks)))
   "</ul>"
   "</div>"))

(defun resume--html-skill (skill)
  ""
  (format "<li><span class=\"skillHeading\">%s:</span> <span class=\"skillDescription\">%s</span>"
          (plist-get skill :skill)
          (plist-get skill :note)))

(defun resume--html-education (education)
  ""
  (format "
        <div class=\"jobBlock\">
           <span class=\"title\">%s</span>
           <span class=\"location\">%s</span>
           <br>
           <span class=\"company\">%s</span>
           <span class=\"date\">%s</span>
        </div> "

          (plist-get education :university)
          (plist-get education :date)
          (plist-get education :degree)
          (plist-get education :gpa)))

(defun resume--html-section (title)
  ""
  (format "
<div class=\"sectionTitle\">%s</div>
<div class=\"sectionLine\"></div>" title))

(defun resume--html-job (job)
  ""
  (format "
        <div class=\"jobBlock\">
           <span class=\"title\">%s</span>
           <span class=\"location\">%s</span>
           <br>
           <span class=\"company\">%s</span>
           <span class=\"date\">%s</span>
        </div> "

          (plist-get job :title)
          (plist-get job :location)
          (plist-get job :company)
          (string-replace "--" "–" (plist-get job :dates))
          ))

(defun resume--html-contact-info (contact-info)
  ""
  (-let (((&plist :name :email :title :address :github :linkedin :phone) contact-info))

    (concat

     ;; name
     "<div class=\"name\">"
     name
     "</div>"

     (when phone
       (concat
        "<div class=\"contactBlock\">"
        (format "<span class=\"phone\">%s</span>" phone)))

     (when email
       (concat
        (format "<span class=\"divider\"> • </span>")
        (format "<span class=\"email\"><a href=\"mailto:%s\">%s</a></span>" email email)))

     (when github
       (concat
        (format "<span class=\"divider\"> • </span>")
        (format "<span class=\"email\"><a href=\"%s\">Github</a></span>" github)))

     (when linkedin
       (concat
        (format "<span class=\"divider\"> • </span>")
        (format "<span class=\"email\"><a href=\"%s\">Linkedin</a></span>" linkedin)))
     "</div>")))

(defun resume-make-html (contact-info jobs projects skills project-intro educations)
  (f-write-text (string-join
                 `(,(resume--html-header)
                   ,(resume--html-contact-info contact-info)

                   ,(resume--html-section "Skills")
                   "<div style=\"text-align: justify;\">"
                   "<ul>"
                   ,(string-join (mapcar #'resume--html-skill skills) "\n")
                   "</ul>"
                   "</div>"

                   ,(resume--html-section "Job History")
                   ,(string-join (mapcar #'resume--html-job jobs) "\n")

                   ,(resume--html-section "Education")
                   ,(string-join (mapcar #'resume--html-education educations) "\n")

                   ,(resume--html-section "Selected Projects")
                   "<div style=\"text-align: justify;\">"
                   "<div>"
                   ,project-intro
                   "</div>"
                   ,(string-join (mapcar #'resume--html-project projects) "\n")
                   "</div>"
                   ,(resume--html-trailer))
                 "\n") 'utf-8 "resume-gen.html"))

(defun resume--latex-header ()
  (string-join  '(
                  "\\documentclass[11pt,letterpaper]{article}"
                  "\\usepackage{fmtcount}"
                  "\\usepackage{ifthen}"
                  "\\usepackage{anysize}"
                  "\\usepackage{url}"
                  "\\usepackage[colorlinks=false, linkcolor=black, hidelinks, urlcolor=blue,  citecolor=blue, bookmarks=true]{hyperref}"
                  "\\usepackage{xcolor}"
                  "\\usepackage{enumitem}"
                  "\\setlist{leftmargin=6.5mm}"
                  "\\renewcommand{\\familydefault}{\\sfdefault}"
                  "\\renewcommand{\\labelitemi}{$\\bullet$}"
                  "\\renewcommand{\\labelitemii}{-}"
                  "\\renewcommand{\\labelitemiii}{$\\diamond$}"
                  "\\pagenumbering{gobble} % no numbering"
                  "\\marginsize{2cm}{2cm}{0cm}{1cm}"
                  "\\usepackage{environ,etoolbox}"
                  "\\input{jobs.tex}"
                  "\\excludecomment{cv}"
                  "\\definecolor{mygray}{RGB}{31,31,31}"
                  "\\newcommand{\\yearsago}[1]{\\the\\numexpr \\year - #1 \\relax}"
                  "\\begin{document}"
                  "\\color{mygray}"
                  ) "\n"))

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

(defun resume-make (contact-info jobs projects skills project-intro educations)
  "Generate HTML and Latex resume documents."
  (resume-make-latex contact-info jobs projects skills project-intro schools)
  (resume-make-html contact-info jobs projects skills project-intro schools)
  ;; TODO: copy css file over also
  )

(provide 'resume)
;;; resume.el ends here
