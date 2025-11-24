;;; resume-html.el --- provides resume html generation -*- lexical-binding: t; -*-
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

(require 'dash)
(require 'f)

(defvar resume-css
  (f-read-text "resume.css"))

(defvar resume-html-file-name
  "resume.html")

(defvar resume-css-file-name
  "resume.css")

(defvar resume--html-header
  "<html>
<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"resume.css\">
</head>
</body>
<div id=\"resume\">
")

(defvar resume--html-trailer
  "</div></body></html>")

(defun resume-html--convert-tex-hrefs (str)
  (replace-regexp-in-string
   "\\\\href{\\(.*\\)}{\\(.*\\)}"
   "<a href=\"\\1\">\\2</a>"
   str
   ))

(defun resume--html-project (project-info)
  "Format a single PROJECT into an html div."
  (-let (((&plist :project :tasks) project-info))
    (concat
     "<div>"
     (format "<div class=\"projectName\">%s</div>" project)
     "<ul>"
     (apply #'concat
            (mapcar (lambda (x) (format "<li>%s</li>" x)) tasks))
     "</ul>"
     "</div>")))

(defun resume--html-skill (skill-info)
  ""
  (-let (((&plist :skill :note) skill-info))
    (format "<li><span class=\"skillHeading\">%s:</span> <span class=\"skillDescription\">%s</span>"
            skill note)))

(defun resume--html-education (education)
  ""
  (-let (((&plist :university :date :degree :gpa) education))
    (format "
        <div class=\"jobBlock\">
           <span class=\"title\">%s</span>
           <span class=\"location\">%s</span>
           <br>
           <span class=\"company\">%s</span>
           <span class=\"date\">%s</span>
        </div> " university date degree gpa)))

(defun resume--html-section (title)
  ""
  (format "
<div class=\"sectionTitle\">%s</div>
<div class=\"sectionLine\"></div>" title))

(defun resume--html-job (job)
  ""
  (-let (((&plist :title :location :company :dates) job))
    (format "
        <div class=\"jobBlock\">
           <span class=\"title\">%s</span>
           <span class=\"location\">%s</span>
           <br>
           <span class=\"company\">%s</span>
           <span class=\"date\">%s</span>
        </div> "
            title
            location
            company
            (string-replace "--" "–" dates))))

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
  (f-write-text
   (string-join
    `(,resume--html-header
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
      ,(resume-html--convert-tex-hrefs project-intro)
      "</div>"
      ,(string-join (mapcar #'resume--html-project projects) "\n")
      "</div>"
      ,resume--html-trailer) "\n")
   'utf-8
   resume-html-file-name)

  (f-write-text resume-css 'utf-8 resume-css-file-name))

(provide 'resume-html)
;;; resume-html.el ends here
