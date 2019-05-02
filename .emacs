(set-register ?e '(file . "~/code/configs/.emacs"))
;; Package settings
;; Setting `package-enable-at-startup` to nil speeds up startup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; disable UI clutter
(menu-bar-mode -1)
(tool-bar-mode -1)

(visual-line-mode 1)

;; Simpler yes/no propmt
(defalias 'yes-or-no-p 'y-or-n-p)
;; Don't confirm when killing buffer
(global-set-key [remap kill-buffer] #'kill-this-buffer)

;; Remove whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Centralize creation of backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

;; Default Orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)
(setq org-hide-emphasis-markers t)

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
(set-register ?g '(file . "~/Documents/org/gtd.org"))
(set-register ?s '(file . "~/Documents/org/someday.org"))
(set-register ?t '(file . "~/Documents/org/tickler.org"))
(set-register ?r '(file . "~/Documents/org/references"))
(set-register ?b '(file . "~/Documents/Readings/bibliography/references.bib"))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "tickler.org" "Tickler")
                               "* %i%? \n %t")
                              ("m" "Movie to watch" entry
                               (file+headline "references/movies.org" "Movies")
                               "* %i%? :watchme:")
                              ("b" "Book to read" entry
                               (file+headline "references/books.org" "Books")
                               "* %i%? :readme:")
                              ("d" "Diary" entry
                               (file+olp+datetree "diary.org")
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
        ("b" "Berlin House" tags-todo "housing")
        ("d" "Discourse Connectives" tags-todo "DCs&-learn&-@email")
        ("h" "At Home" tags-todo "@home"
           ((org-agenda-overriding-header "Around the House")))
        ("e" "Email" tags-todo "@email"
           ((org-agenda-overriding-header "Writing Email")))
        ("p" "Phone" tags-todo "@phone"
           ((org-agenda-overriding-header "While on Phone")))
        ("g" "Groceries" tags-todo "@grocer"
           ((org-agenda-overriding-header "At the supermarket")))
        ("o" "Offline" tags-todo "@offline"
           ((org-agenda-overriding-header "Things to do offline")))
        ("f" "Finances" tags-todo "finances"
           ((org-agenda-overriding-header "Money money money!")))
        ("r" . "Retresco-related Stuff")
        ("rr" "Retresco Work" tags-todo "rtr&-@team_weekly&-@email&-@office")
	("ro" "At office" tags-todo "@office")
	("re" "Email" tags-todo "rtr&@email")
	("rt" "Team Weekly" tags-todo "@team_weekly")))
(setq org-tag-persistent-alist
    '(
      ;; Cogsys-related stuff
      (:startgrouptag)
      ("cogsys" . ?c)
      (:grouptags)
      ("DCs" . ?d)
      ("robosanta")
      (:endgrouptag)
      ;; Retresco-related stuff
      (:startgrouptag)
      ("rtr" . ?r)
      (:grouptags)
      ("@team_weekly")
      ("laser" . ?a)
      ("ling_svc")
      ("textengine_core")
      (:endgrouptag)
      ;; Other projects/areas of focus
      ("python_intro")
      ("learn" . ?l)
      ("housing" . ?i)
      ("finances" . ?f)
      ("optimize" . ?z)
      ("software" . ?s)
      ;; general contexts
      ("@offline" . ?o)
      ("@office")
      ("@home" . ?h)
      ("@email" . ?e)
      ("@phone" . ?p)
      ("@grocer" . ?g)))

(setq org-todo-keywords '((type "TODO" "WAITING" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WAITING" . (:foreground "blue"))))

;; helm-bibtex config
(setq bibtex-completion-notes-path "~/Documents/Readings/bibliography/notes.org"
      bibtex-completion-bibliography '("~/Documents/Readings/bibliography/references.bib")
      bibtex-completion-library-path "~/Documents/Readings/bibliography/bibtex-pdfs/")
(setq bibtex-completion-additional-search-fields '(keywords))

 (global-set-key (kbd "M-c") 'helm-bibtex)

(setq org-latex-pdf-process
      '("latexmk -dvi- -pdf %f"))

;; pomidor config
(global-set-key (kbd "<f12>") #'pomidor)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

;; this is broken rn...
;; (require 'helm-bibtex)
;; (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
;; (helm-add-action-to-source "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 0)

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
 '(markdown-hide-markup t)
 '(olivetti-body-width 80)
 '(org-agenda-dim-blocked-tasks :invisible)
 '(org-agenda-span 4)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (org-bullets olivetti flyspell-correct ox-gfm evil-surround helm-bibtex evil-colemak-basics pomidor markdown-mode color-theme-solarized evil org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:slant normal :weight normal :height 160 :width normal :family "Linux Biolinum O")))))
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

(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook 'olivetti-mode)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)

;; nicer bullet points for org
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda ()
			   (progn
			     (display-line-numbers-mode 1)
			     (setq display-line-numbers 'visual))))

;; Programming
(use-package aggressive-indent
      :ensure t)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package smartparens
    :ensure t
    :diminish smartparens-mode
    :config
    (add-hook 'prog-mode-hook 'smartparens-mode))
(use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
