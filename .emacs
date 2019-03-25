(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(visual-line-mode 1)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-register ?e '(file . "~/code/configs/.emacs"))

;; Default Orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)

(setq org-agenda-start-on-weekday 0)


;; For consistency with colemak+evil navigation, set "e" for going to previous line
;; At some point consider looking at org-evil for a more comprehensive solution
(require 'org-agenda)
(org-defkey org-agenda-mode-map "e" 'org-agenda-previous-line)
(org-defkey org-agenda-mode-map "p" 'org-agenda-set-effort)



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
                               (file+olp+datetree "~/Documents/org/diary.org")
                               "* Entered on %U\n %i%?")))
(defun current-buffers ()
  (delq nil
    (mapcar (lambda (buffer)
      (buffer-file-name buffer))
      (org-buffer-list 'files t))))
(setq org-refile-targets '((current-buffers :maxlevel . 3)
			   ("~/Documents/org/gtd.org" :maxlevel . 3)
                           ("~/Documents/org/someday.org" :maxlevel . 3)
                           ("~/Documents/org/tickler.org" :maxlevel . 2)
			   ))
(setq org-agenda-custom-commands
      '(
        ("b" "Berlin House Search" tags-todo "housing")
        ("p" "Python Course" tags-todo "+python_intro")
        ("d" "Discourse Connectives" tags-todo "DCs&-learn&-@email")
        ("l" "Learning" tags-todo "learn")
        ("r" "Retresco Work" tags-todo "rtr&-@team_weekly"
           ((org-agenda-overriding-header "Retresco work")))
        ("h" "Housework" tags-todo "@housework"
           ((org-agenda-overriding-header "Around the House")))
        ("e" "Email" tags-todo "@email"
           ((org-agenda-overriding-header "Writing Email")))
        ("g" "Groceries" tags-todo "@grocer"
           ((org-agenda-overriding-header "At the supermarket")))
        ("o" "Office" tags-todo "@office"
           ((org-agenda-overriding-header "At the office")))))
(setq org-tag-persistent-alist
    '(
      ; Cogsys-related stuff
      (:startgrouptag)
      ("cogsys" . ?c)
      (:grouptags)
      ("DCs" . ?d)
      ("robosanta" . ?s)
      (:endgrouptag)
      ; Retresco-related stuff
      (:startgrouptag)
      ("rtr" . ?r)
      (:grouptags)
      ("@team_weekly")
      ("laser" . ?a)
      ("ling_svc")
      ("textengine_core")
      (:endgrouptag)
      ; Other projects/areas of focus
      ("python_intro" . ?p)
      ("learn" . ?l)
      ("housing" . ?i)
      ("finances" . ?f)
      ("optimize" . ?z)
      ; general contexts
      ("@office" . ?o)
      ("@housework" . ?h)
      ("@bank" . ?b)
      ("@email" . ?e)
      ("@grocer" . ?g)))

(setq org-todo-keywords '((type "TODO" "WAITING" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WAITING" . (:foreground "blue"))))

; helm-bibtex config
(setq bibtex-completion-notes-path "~/Dropbox/Readings/bibliography/notes/"
      bibtex-completion-bibliography '("~/Dropbox/Readings/bibliography/references.bib")
      bibtex-completion-library-path "~/Dropbox/Readings/bibliography/bibtex-pdfs/")
(setq bibtex-completion-additional-search-fields '(keywords))

 (global-set-key (kbd "M-c") 'helm-bibtex)

(setq org-latex-pdf-process
      '("latexmk -dvi- -pdf %f"))

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
 '(org-agenda-span 4)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (flyspell-correct ox-gfm evil-surround helm-bibtex evil-colemak-basics pomidor markdown-mode color-theme-solarized evil org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'evil)
(evil-mode 1)
(global-evil-colemak-basics-mode)
(global-evil-surround-mode 1)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; Orgmode settings for evil-surround
;; from: https://stackoverflow.com/a/22418983/4501212
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "/" "/" "/")
(define-and-bind-text-object "*" "\\*" "\\*")
(define-and-bind-text-object "~" "~" "~")
(define-and-bind-text-object "=" "=" "=")
(define-and-bind-text-object "$" "\\$" "\\$") ;; sometimes your have to escape the regex

(add-hook 'org-mode-hook (lambda ()
			   (progn
			   (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
			   (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
			   (push '(?~ . ("~" . "~")) evil-surround-pairs-alist)
			   (push '(?= . ("=" . "=")) evil-surround-pairs-alist)
			     ; this should be added for markdown mode, along with the asterisks and "`"
			     ;(push '(?_ . ("_" . "_")) evil-surround-pairs-alist)
			     )))
