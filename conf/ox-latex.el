;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'ox-latex)

(defun my-latex-filter-backslash (text backend info)
  "Replace $\backslash$ to \\ in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\$\\\\backslash\\\$" "\\\\\\\\" text)))
(add-to-list 'org-export-filter-final-output-functions
             'my-latex-filter-backslash)
(defun my-latex-filter-doller (text backend info)
  "Replace \$ to $ in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\$" "\$" text)))
(add-to-list 'org-export-filter-final-output-functions
             'my-latex-filter-doller)
(defun my-latex-filter-figure (text backend info)
  "Replace .9 to 1 in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\.9" "" text)))
(add-to-list 'org-export-filter-final-output-functions
             'my-latex-filter-figure)

;; report
(add-to-list 'org-latex-classes '(
  "report"
  "\\documentclass[9pt, a4j]{jsarticle}
   \\usepackage[top=20truemm,bottom=20truemm,left=20truemm,right=20truemm]{geometry}
   \\usepackage[dvipdfmx]{hyperref}
   \\usepackage[dvipdfmx]{graphicx}
   \\usepackage{pxjahyper}
   \\usepackage{ascmac,here,txfonts}
   \\usepackage{listings,jlisting}
   \\usepackage{color}
   \\hypersetup{pdfborder={0,0,0}}
   [NO-DEFAULT-PACKAGES]"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; resume
(add-to-list 'org-latex-classes '(
  "resume"
  "\\documentclass[9pt, a4j, twocolumn]{jsarticle}
   \\usepackage[top=30truemm,bottom=25truemm,left=20truemm,right=20truemm]{geometry}
   \\usepackage[dvipdfmx]{hyperref}
   \\usepackage[dvipdfmx]{graphicx}
   \\usepackage{pxjahyper}
   \\usepackage{enumerate}
   \\hypersetup{pdfborder={0,0,0}}
   [NO-DEFAULT-PACKAGES]"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; thesis
(add-to-list 'org-latex-classes '(
  "thesis"
  "\\documentclass{risepaper}
   \\卒論
   \\usepackage{epsbox}
   \\usepackage{makeidx}
   \\usepackage[dvipdfmx]{graphicx}
   \\usepackage[dvipdfmx]{hyperref}
   \\usepackage{pxjahyper}
   \\usepackage{enumerate}
   \\hypersetup{pdfborder={0,0,0}}
   [NO-DEFAULT-PACKAGES]"
  ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ieee
(add-to-list 'org-latex-classes '(
  "ieee"
  "\\documentclass[conference]{IEEEtran}
   \\usepackage[caption=false,font=footnotesize]{subfig}
   \\usepackage[dvipdfmx]{graphicx}
   \\usepackage[dvipdfmx]{hyperref}
   \\usepackage{cite}
   \\usepackage{array}
   \\usepackage{fixltx2e}
   \\usepackage{url}
   \\usepackage{enumerate}
   \\hypersetup{pdfborder={0,0,0}}
   [NO-DEFAULT-PACKAGES]"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; beamer
(add-to-list 'org-latex-classes '(
  "beamer"
  "\\documentclass[dvipdfmx]{beamer}
   [NO-DEFAULT-PACKAGES]"
  org-beamer-sectioning))

;; デフォルトの #+LATEX_CLASS
(setq org-latex-default-class "report")
