;;enable packages to be installed from melpa
(require 'package)

; list the packages you want
(setq package-list '(org-journal eyebrowse org-ref))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))





;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.milkbox.net/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#ffffff" "#37ffff" "#e074e3" "#3732ff" "#ffff0b" "#37ff3c" "#ff400b" "#848088"])
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("4e0c46bacfa18716370f52f4e1acda19ddeced16caac66afc33fa0d0161df111" "5a28123387ad126d39f4f1200f953fe6a3a5397c35efc9628be572a1a167ebe0" "f3b2a32914eebbc95b08f04d4377ed8b51205037082a5f20686c0c1aad2cce89" "5f1bd7f67dc1598977e69c6a0aed3c926f49581fdf395a6246f9bc1df86cb030" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" default)))
 '(hl-sexp-background-color "#33323e")
 '(org-capture-templates
   (quote
    (("b" "For recording those lightbulb moments" entry
      (file "~/org/brainstorm.org")
      "" :prepend t))))
 '(org-journal-dir "~/daily_tasks/")
 '(org-journal-file-format "%m.%d.%Y.org")
 '(package-selected-packages
   (quote
    (auctex-lua org-edit-latex org-re-reveal-ref org-ref pdf-tools eww-lnum use-package eyebrowse org-journal dracula-theme leuven-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq default-directory "~/Notes")
(cd "~/Notes")


;;prevent emacs from saving to current directory
(setq auto-save-interval 20)
;;(setq mySaveVariable

;;(setq auto-save-file-name-transforms '((".*" (concat (file-name-directory buffer-file-name) ".emacs-saves/\\2") t)))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/\\2" t)))





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


;;set file header for each daily notes
(setq org-journal-file-header (concat
			       "#+TODO: TODO(t) STARTED(s) WAITING(w) | DONE(d) CANCELED(c)\n"
			       "#+TAGS: { @school(c)  @housekeeping(h) @stuff(s) }\n"
			       "#+STARTUP: indent\n"))





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

(setq org-agenda-files (list "~/Notes/work.org"
                             "~/Notes/school.org" 
                             "~/Notes/home.org"))

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

(setq org-directory "~/org-mode")



(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files '("~/org-mode/"))

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
      '(("d" "Daily agenda and all TODOs"
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


(setq org-capture-templates
      '(
        ("s" "School Task Menu")
 	("sl"				; key
	  "School Task With Link"             ; description
	  entry              ; type
	  (file+headline "~/org-mode/school_tasks.org" "Tasks")       ; target
	  "* TODO %^{Todo} %^{Hours} %? %^g%^g \n:PROPERTIES:\n:Created: %U\nLink: %a\n:END:\n\n"  ; template
	  :prepend t        ; properties
	  :empty-lines 1    ; properties
	  :created t        ; properties
	  )
	

        ("sk"				; key
	  "School Task Without Link"             ; description
	  entry              ; type
	  (file+headline "~/org-mode/school_tasks.org" "Tasks")       ; target
	  "* TODO %^{Todo} %? %^g%^g \n:PROPERTIES:\n:Created: %U\n:END:\n\n"  ; template
	  :prepend t        ; properties
	  :empty-lines 1    ; properties
	  :created t        ; properties
	  )

	("n"				; key
	  "School Task Without Link"            ; description
	  entry              ; type
	  (file+headline "~/org-mode/school_tasks.org" "Tasks")       ; target
	  "* TODO %^{Todo} %? %^g%^g \n:PROPERTIES:\n:Created: %U\n:END:\n\n"  ; template
	  :prepend t        ; properties
	  :empty-lines 1    ; properties
	  :created t        ; properties
	  )
	
	
      

      ))


(setq org-ref-bibliography-notes "~/org-mode/ref/notes.org"
      org-ref-default-bibliography '("~/org-mode/ref/master.bib")
      org-ref-pdf-directory "~/org-mode/ref/pdfs/")
