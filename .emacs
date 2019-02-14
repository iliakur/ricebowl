(tool-bar-mode -1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(visual-line-mode 1)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

(require 'evil)
(evil-mode 1)

(set-register ?e '(file . "~/.emacs"))

;; Default Orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)


;; Orgmode + GTD
;; Inspiration: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-directory (file-name-as-directory "~/Documents/org"))

(setq org-agenda-files
      (mapcar
	(lambda (name)
	  (concat org-directory name ".org"))
	(list "inbox" "gtd" "tickler")))

(set-register ?i '(file . "~/Documents/org/inbox.org"))
(set-register ?p '(file . "~/Documents/org/gtd.org"))
(set-register ?s '(file . "~/Documents/org/someday.org"))
(set-register ?t '(file . "~/Documents/org/tickler.org"))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "tickler.org" "Tickler")
                               "* %i%? \n %t")
                              ("d" "Diary" entry
                               (file+datetree "~/Documents/org/diary.org")
                               "* Entered on %U\n %i%?")))
(setq org-refile-targets '(("~/Documents/org/gtd.org" :maxlevel . 3)
                           ("~/Documents/org/someday.org" :maxlevel . 3)
                           ("~/Documents/org/tickler.org" :maxlevel . 2)
			   ))
(setq org-agenda-custom-commands
      '(
        ("b" "Banking" tags-todo "@bank"
           ((org-agenda-overriding-header "When banking")))
        ("c" . "CogSys Masters") 
        ("cc" "CogSys general" tags-todo "@cogsys"
           ((org-agenda-overriding-header "CogSys Work")))
        ("cp" "Python Course" tags-todo "@cogsys+python_intro"
           ((org-agenda-overriding-header "CogSys Work")))
        ("ci" "IM" tags-todo "@cogsys+im"
           ((org-agenda-overriding-header "CogSys Work")))
        ("r" "Retresco Work" tags-todo "@rtr&-@team_weekly"
           ((org-agenda-overriding-header "Retresco work")))
        ("h" "Housework" tags-todo "@housework"
           ((org-agenda-overriding-header "Around the House")))
        ("e" "Email" tags-todo "@email"
           ((org-agenda-overriding-header "Writing Email")))
        ("g" "Groceries" tags-todo "@grocer"
           ((org-agenda-overriding-header "At the supermarket")))
	))
(setq org-tag-persistent-alist
    '(
      ("@office" . ?o)
      ("@team_weekly")
      ("@rtr" . ?r)
      ("@housework" . ?h)
      ("@cogsys" . ?c)
      ("@bank" . ?b)
      ("@email" . ?e)
      ("@grocer" . ?g)))

(setq org-todo-keywords '((type "TODO" "WAITING" "|" "DONE")))
(setq org-todo-keyword-faces
      '(
        ("TODO" . org-warning) ("WAITING" . (:foreground "blue"))
        ))

; helm-bibtex config
(setq bibtex-completion-notes-path "~/Dropbox/Readings/bibliography/notes/"
      bibtex-completion-bibliography '("~/Dropbox/Readings/bibliography/references.bib")
      bibtex-completion-library-path "~/Dropbox/Readings/bibliography/bibtex-pdfs/")
(setq bibtex-completion-additional-search-fields '(keywords))

 (global-set-key (kbd "M-c") 'helm-bibtex)

(setq org-latex-pdf-process
      '("latexmk -xelatex %f"))

; pomidor config
(global-set-key (kbd "<f12>") #'pomidor)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

; this is broken rn...
; (require 'helm-bibtex)
; (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
; (helm-add-action-to-source "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-dim-blocked-tasks :invisible)
 '(org-agenda-span 3)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-enforce-todo-dependencies t)
 '(package-selected-packages
   (quote
    (evil-surround helm-bibtex evil-colemak-basics pomidor markdown-mode color-theme-solarized evil org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
