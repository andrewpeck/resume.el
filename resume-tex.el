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

(defvar resume-tex-file-name
  "resume.tex")

(setq resume-jobs
      (f-read-text "jobs.tex"))

(setq resume--latex-header
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
  (thread-last str
               (string-replace "&" "\\&")
               (s-replace-regexp "[\n\r\s]+" " ")))

(defun resume--latex-project (project)
  "Format a single PROJECT as LaTeX."
  (concat
   (format "\\project{%s}\n{%s}\n"
           (resume--latexify (plist-get project :project))
           (resume--latexify (plist-get project :skills)))
   "\\begin{itemize}\n"
   (apply #'concat
          (mapcar (lambda (x) (format "\\item %s\n" (resume--latexify x)))
                  (plist-get project :tasks)))
   "\\end{itemize}\n"))

(defun resume--latex-skill (skill)
  "Format a single SKILL as LaTeX."
  (format "\\item \\textbf{%s:} %s\n"
          (resume--latexify (plist-get skill :skill))
          (resume--latexify (plist-get skill :note))))

(defun resume--latex-education (education)
  "Format a single EDUCATION as LaTeX.

Re-uses the same base format as a job."
  (resume--latex-job
   `(:company  ,(plist-get education :university)
     :location ,(plist-get education :date)
     :title    ,(plist-get education :degree)
     :dates    ,(plist-get education :gpa))))

(defun resume--latex-job (job)
  "Format a single latex JOB."
  (format "\\job\n{%s}\n{%s}\n{%s}\n{%s}\n"
          (plist-get job :company)
          (plist-get job :location)
          (plist-get job :title)
          (plist-get job :dates)))

(defun resume--latex-href (link name)
  (format "\\href{%s}{%s}" link name))

(defun resume--latex-contact-info (contact-info)
  ""
  (string-join
   `(
     "\\begin{center}"
     ,(format "{\\huge \\textbf{%s}} \\\\"    (plist-get contact-info :name))
     "\\vspace {0.1em}"
     ,(format  "{\\large %s} \\\\"    (plist-get contact-info :title))
     "\\vspace{5pt}"
     ,(plist-get contact-info :address)
     ,(string-join

       (list
        (plist-get contact-info :phone)
        (resume--latex-href (concat "mailto:" (plist-get contact-info :email))
                            (plist-get contact-info :email))
        (resume--latex-href (plist-get contact-info :github) "Github")
        (resume--latex-href (plist-get contact-info :linkedin) "LinkedIn"))

       "~~\\textbullet~~")

     "\\vspace{4pt}"
     "\\end{center}") "\n")))

(defun resume-make-latex (contact-info jobs projects skills project-intro educations)
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

      "\\heading{Selected Projects}"
      ,(string-join (mapcar #'resume--latex-project projects) "\n")
      "\\end{document}\n") "\n")
   'utf-8
   resume-tex-file-name))

(provide 'resume-tex)
;;; resume-tex.el ends here
