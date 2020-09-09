;;enable packages to be installed from melpa
(require 'package)
					; list the packages you want
(setq package-list '(org-journal eyebrowse org-ref pdf-tools org-noter magit))

					; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("magit" . "")
			 ("MELPA" . "http://melpa.org/packages/")
			 ))

(require 'epa-file)
(epa-file-enable)
					; activate all the packages (in particular autoloads)
(package-initialize)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>") 'hs-hide-block)

(defun my_hideshow-ignore-setup-failure() (ignore-errors (hs-minor-mode))) (define-globalized-minor-mode global-hs-minor-mode   hs-minor-mode my_hideshow-ignore-setup-failure)
(my_hideshow-ignore-setup-failure)


					; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

					; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq elpy-rpc-python-command "python")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#ffffff" "#37ffff" "#e074e3" "#3732ff" "#ffff0b" "#37ff3c" "#ff400b" "#848088"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#93E0E3")
 '(cua-normal-cursor-color "#DCDCCC")
 '(cua-overwrite-cursor-color "#F0DFAF")
 '(cua-read-only-cursor-color "#7F9F7F")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "4e0c46bacfa18716370f52f4e1acda19ddeced16caac66afc33fa0d0161df111" "5a28123387ad126d39f4f1200f953fe6a3a5397c35efc9628be572a1a167ebe0" "f3b2a32914eebbc95b08f04d4377ed8b51205037082a5f20686c0c1aad2cce89" "5f1bd7f67dc1598977e69c6a0aed3c926f49581fdf395a6246f9bc1df86cb030" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" default)))
 '(fci-rule-color "#4F4F4F")
 '(highlight-changes-colors (quote ("#DC8CC3" "#bbb0cb")))
 '(highlight-symbol-colors
   (quote
    ("#680f63eb5998" "#54db645064d0" "#6097535f5322" "#5c2859a95fa1" "#4ede55f24ea4" "#64dd5979525e" "#530060d16157")))
 '(highlight-symbol-foreground-color "#FFFFEF")
 '(highlight-tail-colors
   (quote
    (("#4F4F4F" . 0)
     ("#488249" . 20)
     ("#5dacaf" . 30)
     ("#57a2a4" . 50)
     ("#b6a576" . 60)
     ("#ac7b5a" . 70)
     ("#aa5790" . 85)
     ("#4F4F4F" . 100))))
 '(hl-bg-colors
   (quote
    ("#b6a576" "#ac7b5a" "#9f5c5c" "#aa5790" "#85749c" "#57a2a4" "#5dacaf" "#488249")))
 '(hl-fg-colors
   (quote
    ("#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F")))
 '(hl-paren-colors (quote ("#93E0E3" "#F0DFAF" "#8CD0D3" "#bbb0cb" "#7F9F7F")))
 '(hl-sexp-background-color "#33323e")
 '(lsp-ui-doc-border "#FFFFEF")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#488249" "#95d291" "#57a2a4" "#93E0E3" "#DC8CC3" "#bbb0cb")))
 '(org-agenda-files nil)
 '(org-agenda-sticky t)
 '(org-capture-templates
   (quote
    (("b" "For recording those lightbulb moments" entry
      (file "~/org/brainstorm.org")
      "" :prepend t))))
 '(org-journal-dir "~/daily_tasks/")
 '(org-journal-file-format "%m.%d.%Y.org")
 '(package-selected-packages
   (quote
    (org-noter interleave magit-popup auctex-cluttex latex-preview-pane auctex-latexmk elpy powershell vimish-fold hideshowvis rust-mode solarized-theme magit popup-complete auctex-lua org-edit-latex org-re-reveal-ref org-ref pdf-tools eww-lnum use-package eyebrowse org-journal dracula-theme leuven-theme)))
 '(pos-tip-background-color "#4F4F4F")
 '(pos-tip-foreground-color "#FFFFEF")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#7F9F7F" "#4F4F4F" 0.2))
 '(term-default-bg-color "#3F3F3F")
 '(term-default-fg-color "#DCDCCC")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#CC9393")
     (40 . "#df51b97ca1ae")
     (60 . "#e83dcc9aa8b1")
     (80 . "#F0DFAF")
     (100 . "#cadbca369f51")
     (120 . "#b7fbbf79973e")
     (140 . "#a52cb4cc8f3f")
     (160 . "#9260aa2d8754")
     (180 . "#7F9F7F")
     (200 . "#87dbb4dba003")
     (220 . "#8b6ebfadb0a1")
     (240 . "#8e96ca9fc17c")
     (260 . "#914ed5b0d293")
     (280 . "#93E0E3")
     (300 . "#90c5da6cdd6f")
     (320 . "#8f5dd735da39")
     (340 . "#8df4d401d704")
     (360 . "#8CD0D3"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#3F3F3F" "#4F4F4F" "#9f5c5c" "#CC9393" "#488249" "#7F9F7F" "#b6a576" "#F0DFAF" "#57a2a4" "#8CD0D3" "#aa5790" "#DC8CC3" "#5dacaf" "#93E0E3" "#DCDCCC" "#6F6F6F")))
 '(xterm-color-names
   ["#4F4F4F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#fffff6"])
 '(xterm-color-names-bright
   ["#3F3F3F" "#DFAF8F" "#878777" "#6F6F6F" "#DCDCCC" "#bbb0cb" "#FFFFEF" "#FFFFFD"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; pdf tools
(use-package pdf-tools   
:ensure t 
:config   (pdf-tools-install)   
(setq-default pdf-view-display-size 'fit-page))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (latex . t)))

(setq doc-view-ghostscript-program "C:/Program Files/gs/gs9.52/bin")

(when (string-equal system-type "windows-nt")
  (setq exec-path '(
		    "C:/Program Files/MiKTeX/miktex/bin/x64"
		    "C:/Program Files/Git/bin"
	    )
	)
  )

(define-key global-map (kbd "C-c g") 'magit-status)

(latex-preview-pane-enable) ; automatically compiles LaTeX files

(use-package pdf-tools

:config (pdf-tools-install))
;; allow help for function under cursor
(require 'popup)

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (save-window-excursion
                        (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string)))))
    (popup-tip description
               :point (point)
               :around t
               :height 20
               :scroll-bar t
               :margin t)))

(define-key global-map "\C-x9" 'describe-thing-in-popup)

(defun capitalize-backwards ()
            "Upcase the last letter of the word at point."
            (interactive)
            (backward-word 1)
            (forward-word 1)
            (backward-char 1)
            (capitalize-word 1))

;;prevent emacs from saving to current directory
(setq auto-save-interval 20)
;;(setq mySaveVariable

;;(setq auto-save-file-name-transforms '((".*" (concat (file-name-directory buffer-file-name) ".emacs-saves/\\2") t)))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/\\2" t)))

;; (use-package magit
;;   :config
;;   (global-set-key (kbd "C-c m") 'magit-status))



;;makes shift tab work
(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(load "auctex.el" nil t t)

;;set file header for each daily notes
(setq org-journal-file-header (concat
			       "#+TODO: TODO(t) STARTED(s) WAITING(w) | DONE(d) CANCELED(c)\n"
			       "#+TAGS: { @school(c)  @housekeeping(h) @stuff(s) }\n"
			       "#+STARTUP: indent\n"))



(setq org-latex-create-formula-image-program 'imagemagick)

;;(concat (file-name-directory buffer-file-name) "asdf")

;; no startup msg

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(use-package org-journal  
  :ensure t
  :custom
  (org-journal-dir "~/daily_tasks/")
  (org-journal-file-format "%m.%d.%Y.org"))


;;org mode keybinding for showing all nodes
(add-hook 'org-mode-hook (lambda ()
                           (local-set-key (kbd "C-c s") 'org-show-subtree)))

(require 'org-journal)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Dropbox/org-mode/tasks.org"
                             "~/Dropbox/org-mode/school_tasks.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)


(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

(setq org-directory "~/Dropbox/org-mode")



(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files '("~/Dropbox/org-mode/"))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("w" todo "WAITING")

	("d" "Daily agenda and all TODOs" 
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))

	("e" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))


	
	)
      )

(add-hook 'org-capture-mode-hook
          (lambda ()
            (save-restriction
              (widen)
              (setq-local org-tag-alist (org-get-buffer-tags)))))



(setq org-agenda-custom-commands '(
  ("1" "Events" agenda "display deadlines and exclude scheduled" (
    (org-agenda-span 'week)
    (org-agenda-time-grid nil)
    (org-agenda-show-all-dates nil)
    (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
    (org-deadline-warning-days 1) ))
    ))

(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

					;example org-capture-templates that is rewritten ; for personal inspiration
(setq org-capture-templates
      '(("d" ; key
	 "Demo template"  ; description
	 entry
	 (file+headline "demo.org" "Our first heading")
	 "* DEMO TEXT %?")
	("p" ; key
	 "Prompt us for input"  ; description
	 entry
	 (file+headline "demo.org" "Our first heading")
	 "* %^{Please write here} %?")
	
        ("o" ; key
	 "Options in prompt"  ; description
	 entry
	 (file+headline "demo.org" "Our second heading")
	 "* %^{Please write here |ONE|TWO|THREE} %?")

	("t" "Task with a date" entry
	 (file+headline "demo.org" "Scheduled tasks")
	 "* BLA\n %i %?")

	
	("f"                ; key
	 "Todo"             ; description
	 entry              ; type
	 (file+headline "~/org-mode/notes.org" "tasks")       ; target
	 "* TODO [#B] %^{Todo} %^g \n:PROPERTIES:\n:Created: %U\nLink: %a\n:END:\n\n%?"  ; template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties
	 )
	
	))

					; template-factor is a wrapper for org-templates reducing redundant code
(defun template-factor (key description fileName header text)
  `(,key
    ,description
    entry
					; (file+headline ,(concat "~/org-mode/" fileName) ,header)
    (file+headline ,(concat org-directory "/" fileName) ,header)
    ,text
    :prepend t
    :empty-lines 1
    :created t)
  )

					; org-capture-templates for quick reminders / events
					; please note that even though 5 astericks are specified, org-mode automatically sets the astericks to be 1 + astericks of specified heading (in this case Tasks)
(setq org-capture-templates
      `(
	;;;; Uncomment this during school
	,(template-factor
	  "h"               ; key
	  "Miscellaneous Note With Link" ; description
	  "notes.org" ; file
	  "Notes" "***** %^{Project} %^{Description} \n:PROPERTIES:\n:Created: %U\nLink: %a\n:END:\n\n" ; text
	  )
	("s" "School Task Menu")
	,(template-factor
	  "sl"               ; key
	  "School With Link" ; description
	  "school_tasks.org" ; file
	  "Tasks" "***** TODO %^{Todo} %? %^g%^g \n:PROPERTIES:\n:Created: %U\nLink: %a\n:END:\n\n" ; text
	  )
	,(template-factor
	  "sk"
	  "School Without Link"
	  "school_tasks.org"
	  "Tasks"
	  "***** TODO %^{Todo} %? %^g%^g \n:PROPERTIES:\n:Created: %U\n:END:\n\n"
	  )
	,(template-factor
	 "s"
	 "School Tasks"
	 "school_tasks.org"
	 "TASKS"
	 "***** TODO %^{Todo} %? %^g \n:PROPERTIES:\n:Created: %U\n:END:\n\n"
	 )
	,
	(template-factor
	  "n"
	  "Generic Task"
	  "tasks.org"
	  "TASKS"
	  "***** TODO %^{Todo} %? %^g \n:PROPERTIES:\n:Created: %U\n:END:\n\n"
	  )
	,(template-factor
	  "z"
	  "Testing template-factorfff"
	  "template-factor.org"
	  "template-factorf"
	  "***** %^{template-factor-prompt}"
	  )
	("p" "Insert Useful Links")
	,(template-factor
	  "pe"
	  "Emacs Resources"
	  "resources.org"
	  "Emacs"
	  "***** %^{Description} \n:PROPERTIES:\n:Created: %U\n:ConfigLink: %a\n:WebLink: %^{Website URL} \n:END:\n\n"  ; template
	  )
	,(template-factor
	  "pm"
	  "Miscellaneous Resources"
	  "resources.org"
	  "Miscellaneous"
	  "***** %^{Description} \n:PROPERTIES:\n:Created: %U\n:WebLink: %^{Website URL} \n:END:\n\n"
	  )
	,(template-factor
	  "j"
	  "Journal Entry"
	  "journal.gpg"
	  "Journal"
	  "***** %U\n %^{Description}\n\n "
	  )
	,(template-factor
	  "f"
	  "Garbage Ideas"
	  "garbage.org"
	  "Stupid"
	  "***** %U\n %^{Description} \n:PROPERTIES:\n:Created: %U\n:END:\n\n "
	  )
	))


;; Enable visual line mode (word wrapping and indents for org mode
(defun turn-on-visual-line-mode () (visual-line-mode 1))  
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)  


;; change save directory of files
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; change backups directory of files
(setq backup-directory-alist '(("." . "~/.emacs-backup")))

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)	
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
					;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
					;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))


(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode))
	      auto-mode-alist))

(setq default-directory "~/")
(cd "~/")

(defun org-execute-file-search-in-elisp (s)
  (when (eq major-mode 'emacs-lisp-mode)
    
    )
  )

(add-hook 'org-execute-file-search-functions 'org-execute-file-search-in-elisp)
					      



;; save-hook-function
;; (defun save-hook-test ()
;;   (message "It works!")
;;   )

;; (add-hook 'after-save-hook 'save-hook-test)


;; (setq org-ref-bibliography-notes "~/org-mode/ref/notes.org"
;;       org-ref-default-bibliography '("~/org-mode/ref/master.bib")
;;       org-ref-pdf-directory "~/org-mode/ref/pdfs/")



;; Changes theme based on opened file, however, it is kinda clunky 
;; (defun my-set-theme-on-mode ()
;;   "set background color depending on file suffix"
;;   (interactive)
;;   (let ((fileNameSuffix (file-name-extension (buffer-file-name) ) ))
;;     (cond
;;      ((string= fileNameSuffix "el" ) (set-background-color "honeydew"))
;;      ((string= fileNameSuffix "txt" ) (set-background-color "cornsilk"))
;;      ((string= fileNameSuffix "org" ) (set-background-color "adwaita"))
;;      (t (message "%s" "no match found"))
;;      )
;;     ))

;;(add-hook 'find-file-hook 'my-set-theme-on-mode)
;;(default-directory)
;;(buffer-file-name (current-buffer))
