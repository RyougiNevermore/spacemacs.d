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
    (helm-company :toggle (configuration-layer/package-usedp 'helm))
    yasnippet
    ;;smartparens
    hippie-exp
  )
)

;; init company
(defun private-completion/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-echo-delay 0
            company-minimum-prefix-length 1
            company-tooltip-limit 12
            company-require-match t
            company-dabbrev-ignore-case t
            company-dabbrev-downcase t
      )

      ;;(add-hook 'company-completion-started-hook 'company-turn-off-fci)
      ;;(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      ;;(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
    )
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")
      ;; aligns annotation to the right hand side
      (setq company-tooltip-align-annotations t)

      ;; (setq company-backends (mapcar #'private//company-backend-with-yas company-backends))
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
        ;;(setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
        (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
        (unless (eq private-completion-enable-help-tooltip nil)
          (company-quickhelp-mode 1)
          (setq company-quickhelp-delay 0.5)
        )
      )
    )
  )
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
            yas-wrap-around-region t)
      ;; on multiple keys, fall back to completing read
      ;; typically this means helm
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map (kbd "\s-\t") 'yas-next-field)
      ;; configure snippet directories
      (setq yas-snippet-dirs private-completion-private-snippets-directory)

      (spacemacs/add-to-hooks 
        'private/load-yasnippet 
        '(prog-mode-hook
          markdown-mode-hook
          org-mode-hook)
      )

      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty"
      )

      (spacemacs/add-to-hooks
        'private/force-yasnippet-off 
        '(term-mode-hook
          shell-mode-hook
          eshell-mode-hook)
      )
    )
    :config 
    (spacemacs|diminish yas-minor-mode " ⓨ" " y")
  )
)


(defun private-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))
          (when (configuration-layer/package-usedp 'yasnippet)
          ;; Try to expand yasnippet snippets based on prefix
           (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
          )
)

(defun auto-completion/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (add-hook 'yas-before-expand-snippet-hook
              #'spacemacs//smartparens-disable-before-expand-snippet)
    (add-hook 'yas-after-exit-snippet-hook
              #'spacemacs//smartparens-restore-after-exit-snippet)
  )
)

;;; packages.el ends here
