;;; resume-tex.el --- provides resume latex generation -*- lexical-binding: t; -*-
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
(require 'subr-x)

(defvar resume-tex-file-name
  "resume.tex")

(defvar resume-jobs
  (f-read-text (expand-file-name "jobs.tex" (file-name-directory (or load-file-name buffer-file-name)))))

(defvar resume--latex-header
      (string-join
       `("\\documentclass[11pt,letterpaper]{article}"
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
         "\\definecolor{mygray}{RGB}{31,31,31}"
         ,resume-jobs
         "\\begin{document}"
         "\\color{mygray}"
         "\n"
         )
       "\n"))

(defun resume--latexify (str)
  "Transform STR by replacing special characters for LaTeX compatibility."
  (thread-last str
               (string-replace "&" "\\&")
               (s-replace-regexp "[\n\r\s]+" " ")))

(defun resume--latex-project (project-info)
  "Format a single PROJECT-INFO as LaTeX."
  (-let (((&plist :project :skills :tasks) project-info))
    (concat
     (format "\\project{%s}\n{%s}\n"
             (resume--latexify project)
             (resume--latexify skills))
     "\\begin{itemize}[noitemsep]\n"
     (apply #'concat
            (mapcar (lambda (x) (format "\\item %s\n" (resume--latexify x))) tasks))
     "\\end{itemize}\n")))

(defun resume--latex-skill (skill-info)
  "Format a single SKILL-INFO as LaTeX."
  (-let (((&plist :skill :note) skill-info))
    (format "\\item \\textbf{%s:} %s\n"
            (resume--latexify skill)
            (resume--latexify note))))

(defun resume--latex-education (education)
  "Format a single EDUCATION as LaTeX.

Re-uses the same base format as a job."
  (-let (((&plist :university :date :degree :gpa) education))
    (resume--latex-job
     `(:company  ,university
       :location ,date
       :title    ,degree
       :dates    ,gpa))))

(defun resume--latex-job (job)
  "Format a single latex JOB."
  (-let (((&plist :company :location :title :dates) job))
    (format "\\job\n{%s}\n{%s}\n{%s}\n{%s}\n"
            company location title dates)))

(defun resume--latex-href (link name)
  "Return a LaTeX href string using LINK and NAME.

Format the string as `\href{LINK}{NAME}'."
  (format "\\href{%s}{%s}" link name))

(defun resume--latex-contact-info (contact-info)
  "Generate LaTeX formatted contact information from CONTACT-INFO plist.

CONTACT-INFO should be a plist containing the keys :name, :title, :address,
:phone, :email, :github, :linkedin, and :projects. Returns a LaTeX string
centered and formatted with the provided details, including hyperlinks
for email, GitHub, LinkedIn, and projects."
  (-let (((&plist :name :title :address :phone :email :github :linkedin :projects) contact-info))
    (string-join
     `(
       "\\begin{center}"
       ,(format "{\\huge \\textbf{%s}} \\\\" name)
       "\\vspace {0.1em}"
       ,(format  "{\\large %s} \\\\" title)
       "\\vspace{5pt}"
       ,address "\n"
       ,(string-join

         (list
          phone
          (resume--latex-href (concat "mailto:" email) email)
          (resume--latex-href github "Github")
          (resume--latex-href linkedin "LinkedIn")
          (resume--latex-href projects "Projects")
          )

         "~~\\textbullet~~")

       "\\vspace{4pt}"
       "\\end{center}") "\n")))

(defun resume-make-latex (contact-info jobs projects skills project-intro educations)
  "Generate a LaTeX formatted resume using given details.

CONTACT-INFO, JOBS, PROJECTS, SKILLS, and EDUCATIONS are used to fill in
the respective sections of the resume. PROJECT-INTRO is an introductory
text for the projects section. The output is saved to `resume-tex-file-name'."
  (f-write-text
   (string-join
    `(,resume--latex-header
      ,(resume--latex-contact-info contact-info)
      "\\vspace{-10pt}"

      "\\heading{Skills}"
      "\\begin{itemize}"
      ,(string-join (mapcar #'resume--latex-skill skills) "\n")
      "\\end{itemize}"

      "\\heading{Job History}"
      ,(string-join (mapcar #'resume--latex-job jobs) "\n")

      "\\heading{Education}"
      ,(string-join (mapcar #'resume--latex-education educations) "\n")

      "\\heading{Selected Projects}\n\n\\noindent"
      ,project-intro
      ,(string-join (mapcar #'resume--latex-project projects) "\n")
      "\\end{document}\n") "\n")
   'utf-8
   resume-tex-file-name))

(provide 'resume-tex)
;;; resume-tex.el ends here
