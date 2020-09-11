(deftheme jacob
  "Created 2020-09-09.")

(custom-theme-set-variables
 'jacob
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["#ffffff" "#37ffff" "#e074e3" "#3732ff" "#ffff0b" "#37ff3c" "#ff400b" "#848088"])
 '(compilation-message-face (quote default))
 '(org-agenda-files nil)
 '(org-agenda-sticky t)
 '(org-capture-templates (quote (("b" "For recording those lightbulb moments" entry (file "~/org/brainstorm.org") "" :prepend t))))
 '(org-journal-dir "~/daily_tasks/")
 '(org-journal-file-format "%m.%d.%Y.org")
 '(package-selected-packages (quote (spacemacs-theme org-noter interleave magit-popup auctex-cluttex latex-preview-pane auctex-latexmk elpy powershell vimish-fold hideshowvis rust-mode solarized-theme magit popup-complete auctex-lua org-edit-latex org-re-reveal-ref org-ref pdf-tools eww-lnum use-package eyebrowse org-journal dracula-theme leuven-theme)))
 '(custom-safe-themes (quote ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d4da844db52eb7a9d33e3d92f2a7b303c349e55af3a90a5eafecef27cfa5fe20" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "4e0c46bacfa18716370f52f4e1acda19ddeced16caac66afc33fa0d0161df111" "5a28123387ad126d39f4f1200f953fe6a3a5397c35efc9628be572a1a167ebe0" "f3b2a32914eebbc95b08f04d4377ed8b51205037082a5f20686c0c1aad2cce89" "5f1bd7f67dc1598977e69c6a0aed3c926f49581fdf395a6246f9bc1df86cb030" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" default))))

(provide-theme 'jacob)
