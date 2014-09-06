;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'ox-latex)
;; report
(add-to-list 'org-latex-classes '(
  "report"
  "\\documentclass[9pt, a4j]{jsarticle}
   \\usepackage[top=20truemm,bottom=20truemm,left=20truemm,right=20truemm]{geometry}
   \\usepackage[dvipdfm]{hyperref}
   \\usepackage[dvipdfm]{graphicx}
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
   \\usepackage[top=15truemm,bottom=20truemm,left=15truemm,right=15truemm]{geometry}
   \\usepackage[dvipdfm]{hyperref}
   \\usepackage[dvipdfm]{graphicx}
   \\hypersetup{pdfborder={0,0,0}}
   \\pagestyle{empty}
   \\thispagestyle{empty}
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
   \\usepackage[dvips]{graphicx}
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
;; beamer
(add-to-list 'org-latex-classes '(
  "beamer"
  "\\documentclass[dvipdfm]{beamer}
   [NO-DEFAULT-PACKAGES]"
  org-beamer-sectioning))
;; デフォルトの #+LATEX_CLASS
(setq org-latex-default-class "report")
