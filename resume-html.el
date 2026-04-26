;;; resume-html.el --- provides resume html generation -*- lexical-binding: t; -*-
                                        ;
;; Copyright (C) 2025-2026 Andrew Peck

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
;;; Commentary:
;;
;; Functions for generating an html resume.
;;
;;; Code:

(require 'dash)
(require 'f)

(defvar resume-css
  (f-read-text (expand-file-name "resume.css" (file-name-directory (or load-file-name buffer-file-name))))
  "CSS to include in the generated html.")

(defvar resume-html-file-name "resume.html"
  "Name of generated HTMl file.")

(defvar resume-css-file-name
  "resume.css"
  "Name of generated css file.")

(defvar resume--html-header
  "<html>
<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"resume.css\">
</head>
<body>
<div id=\"resume\">\n"
  "Resume HTML header.")

(defvar resume--html-trailer
  "</div></body></html>"
  "Resume HTML trailer.")

(defun resume-html--convert-tex-hrefs (str)
  "Convert LaTeX \\href commands in STR to HTML anchor tags.

Replaces instances of `\\\\href{url}{text}' with `<a href=\"url\">text</a>'."
  (replace-regexp-in-string
   "\\\\href{\\(.*?\\)}{\\(.*?\\)}"
   "<a href=\"\\1\">\\2</a>"
   str))

(defun resume--html-project (project-info)
  "Format a single PROJECT-INFO into an html div.

PROJECT-INFO is a plist containing the project name under :project and
a list of tasks under :tasks. The function returns an HTML div string
with the project name and tasks formatted as a list."
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
  "Format an HTML list item for a skill from SKILL-INFO.

SKILL-INFO is a plist containing `:skill' and `:note' keys. Return a
string representing an HTML list item with the skill heading and its
description."
  (-let (((&plist :skill :note) skill-info))
    (format "<li><span class=\"skillHeading\">%s:</span> <span class=\"skillDescription\">%s</span></li>"
            skill note)))

(defun resume--html-education (education)
  "Format EDUCATION as an HTML block.

EDUCATION is a plist with keys :university, :date, :degree, and :gpa.
Return a string with these details wrapped in HTML tags for display."
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
  "Format an HTML section header with a given TITLE.

Insert the TITLE into a section header using `div' elements for styling."
  (format "
<div class=\"sectionTitle\">%s</div>
<div class=\"sectionLine\"></div>" title))

(defun resume--html-job (job)
  "Format a job entry as an HTML block.

JOB is a plist containing `:title', `:location', `:company', and
`:dates'. Returns a string with the job details formatted in HTML,
replacing `--' with an en dash in the dates."
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
  "Generate HTML contact information block from CONTACT-INFO plist.

CONTACT-INFO is a plist containing :name, :email, :title, :address, :github,
:linkedin, and :phone. Returns a string of HTML with div and span elements
for each available contact attribute. Divider ' • ' is used between elements.
Each online contact link is wrapped in an <a> tag."
  (-let (((&plist :name :email :title :address :github :linkedin :phone) contact-info))

    (concat

     ;; name
     "<div class=\"name\">"
     name
     "</div>"

     (when title
       nil)

     (when address
       nil)

     "<div class=\"contactBlock\">"
     (when phone
       (format "<span class=\"phone\">%s</span>" phone))

     (when email
       (concat
        "<span class=\"divider\"> • </span>"
        (format "<span class=\"email\"><a href=\"mailto:%s\">%s</a></span>" email email)))

     (when github
       (concat
        "<span class=\"divider\"> • </span>"
        (format "<span class=\"email\"><a href=\"%s\">Github</a></span>" github)))

     (when linkedin
       (concat
        "<span class=\"divider\"> • </span>"
        (format "<span class=\"email\"><a href=\"%s\">Linkedin</a></span>" linkedin)))
     "</div>")))

(defun resume-make-html (contact-info jobs projects skills project-intro educations)
  "Generate an HTML resume with provided info.

CONTACT-INFO, JOBS, PROJECTS, SKILLS, PROJECT-INTRO, and EDUCATIONS are used
to construct the respective sections of the resume. The resulting HTML is
written to `resume-html-file-name', and a CSS file is created at
`resume-css-file-name'."

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
