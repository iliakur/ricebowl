;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(graphviz
     json
     html
     yaml
     python
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     better-defaults
     emacs-lisp
     (latex :variables
             latex-enable-folding t
             latex-enable-auto-fill t)
     git
     markdown
     pdf
     restclient
     (org :variables org-enable-github-support t org-enable-reveal-js-support t)
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     (spell-checking :variables =enable-flyspell-auto-completion= t )
     syntax-checking
     ;; version-control
     (keyboard-layout :variables kl-layout 'colemak-hnei)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      olivetti
                                      helm-bibtex
                                      toml-mode
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    pipenv
                                    yapfify
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(sanityinc-solarized-light sanityinc-solarized-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:visual t
                                       :enabled-for-modes
                                       yaml-mode
                                       prog-mode
                                       :disabled-for-modes
                                       text-mode)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (with-eval-after-load 'org
    ;; Appearance: monospaced font in tables and hide markup symbols by default.
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (setq org-hide-emphasis-markers t)

    ;; Orgmode + GTD
    ;; Inspiration: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-directory (file-name-as-directory "~/Documents/org"))
    (setq ik/references-directory
          (file-name-as-directory (expand-file-name "references" org-directory)))

    ;; I try to keep the lists of agenda files and refile targets short for better performance.
    (setq org-agenda-files
          (list (expand-file-name "gtd.org" org-directory)
           (expand-file-name "tickler.org" org-directory)))
    ;; However it's just too convenient to treat currently open buffers as refile targets.
    (defun current-buffers ()
      (delq nil
            (mapcar (lambda (buffer)
                      (buffer-file-name buffer))
                    (org-buffer-list 'files t))))
    (setq org-refile-targets '((current-buffers :maxlevel . 3)
                               ("~/Documents/org/gtd.org" :maxlevel . 3)
                               ("~/Documents/org/someday.org" :maxlevel . 3)
                               ("~/Documents/org/tickler.org" :maxlevel . 2)
                               ("~/Documents/org/groceries.org" :maxlevel . 2)
                               ))

    (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                  (file+headline "inbox.org" "Tasks")
                                  "* TODO %i%?")

                                  ("a" "Work Todo [inbox]" entry
                                   (file+headline "work/inbox.org" "Tasks")
                                   "* TODO %i%?")

                                  ("T" "Tickler" entry
                                  (file+headline "tickler.org" "Tickler")
                                  "* %i%? \n %t")

                                  ("h" "Habit" entry
                                   (file+headline "tickler.org" "Tickler")
                                   "* TODO %? \n SCHEDULED: %t \n :PROPERTIES:\n :STYLE:  habit \n :END:")

                                  ("m" "Movie to watch" entry
                                  (file+headline "references/movies.org" "Movies")
                                  "* TOWATCH %i%?")

                                  ("b" "Book to read" entry
                                  (file+headline "references/books.org" "Reading List")
                                  "* TOREAD %i%?")

                                  ("l" "Listen to this!" entry
                                  (file+headline "references/music.org" "Listen")
                                  "* %i%?")

                                  ("d" "Diary" entry
                                  (file+olp+datetree "diary.org")
                                  "* Entered on %U\n %i%?")))

    ;; Agenda-related settings
    ;; By default I want to just focus on one day.
    (setq org-agenda-span 1)
    ;; I tend to review my tasks on Sunday, so the week starts with that day.
    (setq org-agenda-start-on-weekday 0)
    ;; If I have scheduled a time to deal with a deadline, don't show it in the agenda.
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    (setq org-agenda-tags-todo-honor-ignore-options t)
    (setq org-agenda-todo-ignore-with-date t)
    ;; We don't want to see the children tasks in either agenda or tags matches.
    (setq org-agenda-todo-list-sublevels nil)
    (setq org-agenda-todo-list-sublevels nil)
    ;; We don't want to see blocked tasks either.
    (setq org-agenda-dim-blocked-tasks 'invisible)
    (setq org-agenda-compact-blocks t)
    (defconst ik/work-org-directory (file-name-as-directory (concat org-directory "work")))
    (defconst ik/work-org-files
      (list (expand-file-name "rtr.org" ik/work-org-directory)
            (expand-file-name "gtd.org" ik/work-org-directory)))
    (setq org-agenda-custom-commands
          '(("g" "Groceries" tags-todo "@grocer"
             ((org-agenda-overriding-header "At the supermarket")
              ;; When saving the view we aren't interested in seeing the tag or the prefix.
              (org-agenda-prefix-format " ")
              (org-agenda-remove-tags t)
              (org-agenda-files (list (expand-file-name "groceries.org" org-directory)
                                      (expand-file-name "gtd.org" org-directory))))
              ;; Save to PDF because phones don't always understand txt files...
              ("~/Downloads/groceries.pdf"))
            ("m" "Movies to watch" todo "TOWATCH"
             ((org-agenda-files (list (expand-file-name "movies.org" ik/references-directory)))))

            ("t" . "Personal")
            ("tt" "Personal daily"
             ((agenda "" ((org-agenda-dim-blocked-tasks t)))
              (tags-todo "@email" ((org-agenda-overriding-header "Email/Chat")))
              (tags-todo "@home" ((org-agenda-overriding-header "At home")))
              (tags-todo "-@email&-@home&-@grocer"
                         ((org-agenda-overriding-header "Misc tasks")))))
            ("ti" "Quick inbox review"
             alltodo "" ((org-agenda-files (list (expand-file-name "inbox.org" org-directory)))
                         (org-agenda-todo-ignore-with-date nil)))
            ("tr" "Personal weekly review"
             ((alltodo "" ((org-agenda-overriding-header "Inbox")
                           (org-agenda-files (list (expand-file-name "inbox.org" org-directory)))
                           (org-agenda-todo-ignore-with-date nil)))
              (stuck "" ((org-agenda-files (list (expand-file-name "gtd.org" org-directory)))))
              (tags "weekly+LEVEL=2"
                    ((org-agenda-overriding-header "Backlog for Weekly review")
                     (org-agenda-files (list (expand-file-name "someday.org" org-directory)))))
              (agenda "" ((org-agenda-span 7)
                          (org-agenda-dim-blocked-tasks t)))))

            ("a" . "Work-related")
            ("aa" "Work-related daily"
             ((tags-todo "wip" ((org-agenda-overriding-header "Work in Progress")))
              (agenda "" ((org-agenda-dim-blocked-tasks t)))
              (tags-todo "@email" ((org-agenda-overriding-header "Email/Chat")))
              (tags-todo "-@email&-@office&-wip" ((org-agenda-overriding-header "Downtime tasks")))
              (tags-todo "@office" ((org-agenda-overriding-header "Around the office"))))
             ((org-agenda-files (list (expand-file-name "gtd.org" ik/work-org-directory)))
              (org-refile-targets ik/work-org-files)))
            ("ar" "Work-related weekly review"
             ((alltodo "" ((org-agenda-overriding-header "Inbox")
                           (org-agenda-files
                            (list (expand-file-name "inbox.org" ik/work-org-directory)))
                           (org-agenda-todo-ignore-with-date nil)))
              (stuck "" ((org-agenda-files (list (expand-file-name "gtd.org" ik/work-org-directory)))))
              (agenda "" ((org-agenda-span 7)
                          ;; I currently do my weekly reviews on Wednesday,
                          ;; so I start planning from Thursday onward.
                          (org-agenda-start-on-weekday 4)
                          (org-agenda-dim-blocked-tasks t)
                          (org-agenda-files
                           (list (expand-file-name "gtd.org" ik/work-org-directory))))))
             ((org-refile-targets ik/work-org-files)))))
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
          ("housing" . ?b)
          ("finances" . ?f)
          ("optimize" . ?z)
          ("software" . ?s)
          ;; general contexts and GTD stuff
          ("inbox" . ?i)
          ("@offline" . ?o)
          ("@office")
          ("@home" . ?h)
          ("@email" . ?e)
          ("@phone" . ?p)
          ("@grocer" . ?g)))

    ;; Managing TODOs
    ;; Add keys for faster access to different states instead of cycling through them.
    ;; Source: https://orgmode.org/manual/Fast-access-to-TODO-states.html#Fast-access-to-TODO-states
    ;; Optimized for frequency of use.
    ;; That's why TODO isn't mapped to "t", but DONE is instead.
    ;; Most of the time I'm marking things as done and the keys for that involve pressing "t".
    ;; That being said, marking things directly as TODO is still quite frequent,
    ;; so I use another strong key for that: "s".
    (setq org-todo-keywords '((type "TODO(s)" "WAITING(w)" "|" "DONE(t)")))
    (setq org-todo-keyword-faces '(("TODO" . org-warning) ("WAITING" . (:foreground "blue"))))
    (setq org-enforce-todo-dependencies t)
    (setq org-log-done t)

    ;; Settings related to exporting to other formats.
    (setq org-export-with-toc nil)
    (setq org-export-with-broken-links 'mark)
    (setq org-export-headline-levels 4)
    (setq org-export-backends (quote (ascii html icalendar latex md odt)))
    (setq org-latex-pdf-process
          '("latexmk -dvi- -pdf %f"))
    (setq org-reveal-root "file:~/code/reveal.js")

    (setq org-modules
          (quote
          (org-bbdb org-bibtex org-docview org-gnus org-habit org-drill org-info org-irc org-mhe org-rmail org-w3m)))
  )

  (defun ik/org-cliplink (description)
    "My version of cliplink only prompts for a link description."
    (interactive "s")
    (insert (org-make-link-string (current-kill 0 t) description)))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "ol" 'ik/org-cliplink)

  ;; Org-related keyboard shortcuts for useful toggles.
  (defun ik/toggle-org-pomodoro-sounds ()
    (interactive)
    (setq org-pomodoro-play-sounds (not org-pomodoro-play-sounds))
    (message "Org Pomodoro sounds turned %s"
             (if org-pomodoro-play-sounds "on" "off")))
  (spacemacs/set-leader-keys
    "oh" 'org-habit-toggle-habits
    "op" 'ik/toggle-org-pomodoro-sounds)

  ;; Making org-mode subtree manipulation colemak-friendly.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "si" 'org-demote-subtree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sk" 'org-narrow-to-subtree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sK" 'widen)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sn" 'org-move-subtree-down)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "se" 'org-move-subtree-up)
  ;; Evil-org bindings for navigating subtrees are trickier to remap for colemak hnei
  ;; because they conflict with a lot of existing `g` prefixed bindings.
  ;; So instead we add them to the custom prefix for orgmode.
  ;; For now, keep `gh` as the shortcut for org-up-element.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "on" 'org-forward-element)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oe" 'org-backward-element)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oi" 'org-down-element)

  ;; Open compiled LaTeX documents in PDF-Tools.
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook 'TeX-revert-document-buffer)

  (with-eval-after-load 'evil
    ;; Until this gets fixed, use standard Evil functions instead of evil-org-mode ones:
    ;; https://github.com/Somelauw/evil-org-mode/issues/50
    (evil-define-key 'normal evil-org-mode-map
      (kbd "$") 'evil-end-of-line)
    (evil-define-key 'normal evil-org-mode-map
      (kbd "0") 'evil-beginning-of-line)
    ;; Macro for turning list item into checklist item
    (evil-set-register ?c [?0 ?f ?- ?a ?  ?\[ ?  ?\] escape ?n])
    ;; Splits python/yaml list by placing next item on separate line.
    ;; Note that it's not intended to split lists where items contain spaces!
    (evil-set-register ?a [?f ?  ?s return escape]))

  ;; Yaml Folding
  ;; kudos: https://github.com/jgmize/dotfiles/blob/master/.spacemacs#L501
  (add-hook 'yaml-mode-hook
            (lambda ()
              (outline-minor-mode)
              (define-key yaml-mode-map (kbd "TAB") 'outline-toggle-children)
              (setq outline-regexp "^ *")))

  ;; Olivetti mode
  (add-hook 'text-mode-hook 'olivetti-mode)
  (setq olivetti-body-width 85)

  ;; Variable Pitch
  (add-hook 'text-mode-hook 'variable-pitch-mode)
  (global-set-key (kbd "<f5>")
                  (lambda () (interactive) (variable-pitch-mode)))

  ;; Misc small writing tweaks
  (with-eval-after-load 'markdown
    (setq markdown-hide-markup t))
  ;; HL-line mode is really only use full in text mode buffers where lines regularly wrap.
  (global-hl-line-mode -1)
  (add-hook 'text-mode-hook 'hl-line-mode)

  ;; Simpler yes/no prompt
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Golden-ratio
  (golden-ratio-mode 1)
  ;; Clingo ASP files essentially have prolog syntax, their file extension is .lp
  (add-to-list 'auto-mode-alist '("\\.lp$" . prolog-mode))

  (defun ik/typing-exercises ()
    "Open webpages I need to practice my typing.
Currently that's listenonrepeat, keybr.com and a timer set to 20 minutes."
    (interactive)
    (browse-url "https://listenonrepeat.com/")
    (browse-url "https://www.keybr.com/")
    (browse-url "https://duckduckgo.com/?q=timer+20+minutes"))

  (with-eval-after-load 'auto-complete
    (add-to-list 'ac-modes 'org-mode))
  (with-eval-after-load 'helm-bibtex
    (setq bibtex-completion-notes-path "~/Readings/bibliography/notes.org"
          bibtex-completion-bibliography '("~/Readings/bibliography/references.bib")
          bibtex-completion-library-path "~/Readings/bibliography/bibtex-pdfs/")
    (setq bibtex-completion-additional-search-fields '(keywords))
    (helm-add-action-to-source "Insert BibTex key" 'helm-bibtex-insert-key helm-source-bibtex 0))
  ;; Magit by default displays blame information inline.
  ;; I'm used to having it in a side bar with the code flow uninterrupted.
  (with-eval-after-load 'magit
    (setq magit-blame-styles
          '((margin
            (margin-format " %s%f" " %C %a" " %H")
            ;; This width tends to be better in practice than the default "42".
            (margin-width . 70)
            (margin-face . magit-blame-margin)
            (margin-body-face magit-blame-dimmed))
           (headings
            (heading-format . "%-20a %C %s\n"))
           (highlight
            (highlight-face . magit-blame-highlight))
           (lines
            (show-lines . t)
            (show-message . t))))
    (setq magit-blame-disable-modes
          (add-to-list 'magit-blame-disable-modes 'olivetti-mode))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mmm-mode markdown-toc markdown-mode htmlize gnuplot gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck auto-dictionary ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:slant normal :weight normal :height 160 :width normal :family "Linux Biolinum O"))))
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (graphviz-dot-mode org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mmm-mode markdown-toc markdown-mode htmlize gnuplot gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck auto-dictionary ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(recentf-exclude
   (quote
    ("COMMIT_EDITMSG\\'" "/home/quickbeam/.emacs.d/elpa/develop/" "/home/quickbeam/.emacs.d/.cache/" "*.org_archive"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:slant normal :weight normal :height 160 :width normal :family "Linux Biolinum O")))))
)
