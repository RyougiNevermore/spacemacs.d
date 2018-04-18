;;; packages.el --- private-completion layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: 汪旻翔 <ryougi.nevermore@hotmail.com>
;; URL: https://github.com/RyougiNevermore/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq private-completion-packages
  '(
    company
    (company-quickhelp :toggle private-completion-enable-help-tooltip)
    fuzzy
    (helm-company :toggle (configuration-layer/package-usedp 'helm))
    yasnippet
    smartparens
  )
)

;; init company
(defun private-completion/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 1
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
      )

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
    )
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")

      ;; key bindings
      (defun spacemacs//company-complete-common-or-cycle-backward ()
        "Complete common prefix or cycle backward."
        (interactive)
        (company-complete-common-or-cycle -1)
      )
      (spacemacs//private-completion-set-RET-key-behavior 'company)
      (spacemacs//private-completion-set-TAB-key-behavior 'company)

      (let ((map company-active-map))
        (define-key map (kbd "C-/")   'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d")   'company-show-doc-buffer))
      (add-hook 'spacemacs-editing-style-hook 'spacemacs//company-active-navigation)
      ;; ensure that the correct bindings are set at startup
      (spacemacs//company-active-navigation dotspacemacs-editing-style)

      (setq company-transformers '(spacemacs//company-transformer-cancel
                                   company-sort-by-occurrence))
      (setq company-backends (mapcar #'spacemacs//company-backend-with-yas company-backends))
    )
  )

)

;; company-quickhelp
(defun private-completion/init-company-quickhelp ()
  (use-package company-quickhelp
    :commands company-quickhelp-manual-begin
    :init
    (spacemacs|do-after-display-system-init
      (with-eval-after-load 'company
        (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
        (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
        (unless (eq private-completion-enable-help-tooltip 'manual)
          (company-quickhelp-mode)
        )
      )
    )
  )
)

;; fuzzy
(defun private-completion/init-fuzzy ()
  (use-package fuzzy :defer t)
)

(defun private-completion/init-helm-company ()
  (use-package helm-company
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-/") 'helm-company)
    )
  )
)

;; yasnippet
(defun private-completion/init-yasnippet ()
  (use-package yasnippet
    :commands (yas-global-mode yas-minor-mode)
    :init
    (progn
      ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)
      (setq yas-triggers-in-field t
            yas-wrap-around-region t
            helm-yas-display-key-on-candidate t)
      ;; on multiple keys, fall back to completing read
      ;; typically this means helm
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
      ;; configure snippet directories
      (let* (
              (spacemacs--private-completion-dir
                (configuration-layer/get-layer-local-dir 'private-completion)
              )
              
              (private-yas-dir 
                (if private-completion-private-snippets-directory
                  private-completion-private-snippets-directory
                  (concat 
                  configuration-layer-private-directory
                  "snippets/")
                )
              )

              (spacemacs-layer-snippets-dir 
                (expand-file-name
                "snippets"
                spacemacs--private-completion-dir)
              )

              (dotspacemacs-directory-snippets-dir 
                (when dotspacemacs-directory  
                  (expand-file-name 
                  "snippets"  
                  dotspacemacs-directory)
                )
              )
            )
        (setq yas-snippet-dirs nil)
        ;; ~/.emacs.d/layers/private-completion/snippets
        (push spacemacs-layer-snippets-dir yas-snippet-dirs)
        ;; ~/.emacs.d/elpa/yasnippet-xxxxx/snippets
        (push 'yas-installed-snippets-dir yas-snippet-dirs)
        ;; ~/.spacemacs.d/snippets
        (when dotspacemacs-directory-snippets-dir
          (push dotspacemacs-directory-snippets-dir yas-snippet-dirs)
        )
        ;; arbitrary directories in `private-completion-private-snippets-directory'
        (when private-yas-dir
          (if (listp private-yas-dir)
            (setq yas-snippet-dirs (append yas-snippet-dirs private-yas-dir))
            (push private-yas-dir yas-snippet-dirs)
          )
        )
      )

      (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                          org-mode-hook)
      )

      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty"
      )

      (spacemacs/add-to-hooks
        'spacemacs/force-yasnippet-off '(term-mode-hook
                                        shell-mode-hook
                                        eshell-mode-hook)
      )
    )
    :config 
    (spacemacs|diminish yas-minor-mode " ⓨ" " y")
  )
)

(defun private-completion/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (add-hook 'yas-before-expand-snippet-hook
              #'spacemacs//smartparens-disable-before-expand-snippet)
    (add-hook 'yas-after-exit-snippet-hook
              #'spacemacs//smartparens-restore-after-exit-snippet)
  )
)

;;; packages.el ends here
