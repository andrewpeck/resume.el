;;; test-resume.el --- ERT tests for resume.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Andrew Peck

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'resume-tex)
(require 'resume-html)

;;; resume-tex.el tests

(ert-deftest resume--latexify-ampersand ()
  (should (equal (resume--latexify "foo & bar") "foo \\& bar")))

(ert-deftest resume--latexify-multiple-ampersands ()
  (should (equal (resume--latexify "A & B & C") "A \\& B \\& C")))

(ert-deftest resume--latexify-multiple-spaces ()
  (should (equal (resume--latexify "foo  bar") "foo bar")))

(ert-deftest resume--latexify-no-special-chars ()
  (should (equal (resume--latexify "hello world") "hello world")))

(ert-deftest resume--latex-href ()
  (should (equal (resume--latex-href "https://example.com" "Example")
                 "\\href{https://example.com}{Example}")))

(ert-deftest resume--latex-href-email ()
  (should (equal (resume--latex-href "mailto:a@b.com" "a@b.com")
                 "\\href{mailto:a@b.com}{a@b.com}")))

(ert-deftest resume--latex-job-format ()
  (should (equal (resume--latex-job '(:company "ACME" :location "NYC"
                                      :title "Engineer" :dates "2020--2023"))
                 "\\job\n{ACME}\n{NYC}\n{Engineer}\n{2020--2023}\n")))

(ert-deftest resume--latex-job-contains-fields ()
  (let ((result (resume--latex-job '(:company "ACME" :location "NYC"
                                     :title "Engineer" :dates "2020--2023"))))
    (should (string-match-p "ACME" result))
    (should (string-match-p "NYC" result))
    (should (string-match-p "Engineer" result))
    (should (string-match-p "2020--2023" result))))

(ert-deftest resume--latex-skill-format ()
  (should (equal (resume--latex-skill '(:skill "Python" :note "Intermediate"))
                 "\\item \\textbf{Python:} Intermediate\n")))

(ert-deftest resume--latex-skill-contains-fields ()
  (let ((result (resume--latex-skill '(:skill "Emacs Lisp" :note "Expert"))))
    (should (string-match-p "\\\\textbf{Emacs Lisp:}" result))
    (should (string-match-p "Expert" result))))

(ert-deftest resume--latex-project-contains-project-name ()
  (let ((result (resume--latex-project '(:project "My Project"
                                         :skills "Emacs Lisp"
                                         :tasks ("Task one" "Task two")))))
    (should (string-match-p "My Project" result))
    (should (string-match-p "Emacs Lisp" result))))

(ert-deftest resume--latex-project-contains-tasks ()
  (let ((result (resume--latex-project '(:project "P" :skills "S"
                                         :tasks ("Task one" "Task two")))))
    (should (string-match-p "Task one" result))
    (should (string-match-p "Task two" result))))

(ert-deftest resume--latex-project-uses-itemize ()
  (let ((result (resume--latex-project '(:project "P" :skills "S" :tasks ("T")))))
    (should (string-match-p "\\\\begin{itemize}" result))
    (should (string-match-p "\\\\end{itemize}" result))
    (should (string-match-p "\\\\item" result))))

(ert-deftest resume--latex-education-delegates-to-job ()
  (let ((result (resume--latex-education
                 '(:university "MIT" :date "2015--2019"
                   :degree "B.S. CS" :gpa "3.9"))))
    (should (string-match-p "\\\\job" result))
    (should (string-match-p "MIT" result))
    (should (string-match-p "2015--2019" result))
    (should (string-match-p "B.S. CS" result))
    (should (string-match-p "3.9" result))))

;;; resume-html.el tests

(ert-deftest resume-html--convert-tex-hrefs-single ()
  (should (equal (resume-html--convert-tex-hrefs "\\href{https://example.com}{Example}")
                 "<a href=\"https://example.com\">Example</a>")))

(ert-deftest resume-html--convert-tex-hrefs-passthrough ()
  (let ((plain "No links here."))
    (should (equal (resume-html--convert-tex-hrefs plain) plain))))

(ert-deftest resume-html--convert-tex-hrefs-multiple ()
  (let ((result (resume-html--convert-tex-hrefs
                 "See \\href{https://a.com}{A} and \\href{https://b.com}{B}.")))
    (should (string-match-p "<a href=\"https://a.com\">A</a>" result))
    (should (string-match-p "<a href=\"https://b.com\">B</a>" result))))

(ert-deftest resume--html-section-contains-title ()
  (let ((result (resume--html-section "Experience")))
    (should (string-match-p "Experience" result))
    (should (string-match-p "sectionTitle" result))
    (should (string-match-p "sectionLine" result))))

(ert-deftest resume--html-skill-format ()
  (let ((result (resume--html-skill '(:skill "Python" :note "Expert"))))
    (should (string-match-p "skillHeading" result))
    (should (string-match-p "skillDescription" result))
    (should (string-match-p "Python" result))
    (should (string-match-p "Expert" result))
    (should (string-match-p "<li>" result))))

(ert-deftest resume--html-job-endash ()
  (let ((result (resume--html-job '(:title "Engineer" :location "NYC"
                                    :company "ACME" :dates "2020--2023"))))
    (should (string-match-p "2020–2023" result))))

(ert-deftest resume--html-job-no-bare-double-dash ()
  (let ((result (resume--html-job '(:title "T" :location "L"
                                    :company "C" :dates "2020--2023"))))
    (should-not (string-match-p "2020--2023" result))))

(ert-deftest resume--html-job-contains-fields ()
  (let ((result (resume--html-job '(:title "Engineer" :location "NYC"
                                    :company "ACME" :dates "2020--2023"))))
    (should (string-match-p "Engineer" result))
    (should (string-match-p "NYC" result))
    (should (string-match-p "ACME" result))
    (should (string-match-p "jobBlock" result))))

(ert-deftest resume--html-education-contains-fields ()
  (let ((result (resume--html-education
                 '(:university "MIT" :date "2015--2019"
                   :degree "B.S. CS" :gpa "3.9"))))
    (should (string-match-p "MIT" result))
    (should (string-match-p "B.S. CS" result))
    (should (string-match-p "3.9" result))
    (should (string-match-p "jobBlock" result))))

(ert-deftest resume--html-project-contains-fields ()
  (let ((result (resume--html-project
                 '(:project "My Project" :tasks ("Task one" "Task two")))))
    (should (string-match-p "My Project" result))
    (should (string-match-p "Task one" result))
    (should (string-match-p "Task two" result))
    (should (string-match-p "projectName" result))
    (should (string-match-p "<ul>" result))
    (should (string-match-p "<li>" result))))

(ert-deftest resume--html-contact-info-name ()
  (let ((result (resume--html-contact-info
                 '(:name "John Doe" :email "john@example.com"
                   :phone "555-1234" :github nil :linkedin nil))))
    (should (string-match-p "John Doe" result))
    (should (string-match-p "class=\"name\"" result))))

(ert-deftest resume--html-contact-info-email-link ()
  (let ((result (resume--html-contact-info
                 '(:name "John Doe" :email "john@example.com"
                   :phone "555-1234" :github nil :linkedin nil))))
    (should (string-match-p "mailto:john@example.com" result))
    (should (string-match-p "john@example.com" result))))

(ert-deftest resume--html-contact-info-github-link ()
  (let ((result (resume--html-contact-info
                 '(:name "John Doe" :email nil :phone nil
                   :github "https://github.com/johndoe" :linkedin nil))))
    (should (string-match-p "https://github.com/johndoe" result))
    (should (string-match-p "Github" result))))

(ert-deftest resume--html-contact-info-linkedin-link ()
  (let ((result (resume--html-contact-info
                 '(:name "John Doe" :email nil :phone nil
                   :github nil :linkedin "https://linkedin.com/in/johndoe"))))
    (should (string-match-p "https://linkedin.com/in/johndoe" result))
    (should (string-match-p "Linkedin" result))))

(ert-deftest resume--html-contact-info-omits-nil-github ()
  (let ((result (resume--html-contact-info
                 '(:name "John Doe" :email nil :phone nil
                   :github nil :linkedin nil))))
    (should-not (string-match-p "Github" result))))

(ert-deftest resume--html-contact-info-omits-nil-linkedin ()
  (let ((result (resume--html-contact-info
                 '(:name "John Doe" :email nil :phone nil
                   :github nil :linkedin nil))))
    (should-not (string-match-p "Linkedin" result))))

(provide 'test-resume)
;;; test-resume.el ends here
