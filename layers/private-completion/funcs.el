;;; funcs.el --- private-completion layer func file for Spacemacs.
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



(spacemacs|add-toggle private-completion
    :status
    (if (eq 'company private-completion-front-end)
        (bound-and-true-p company-mode)
    )
    :on
    (progn
        (if (eq 'company private-completion-front-end)
            (company-mode)
        )
        (message "Enabled private-completion (using %S)." private-completion-front-end)
    )
    :off
    (progn
        (if (eq 'company private-completion-front-end)
            (company-mode -1)
        )
        (message "Disabled private-completion.")
    )
    :documentation "Enable private-completion."
    :evil-leader "ta"
)


;; private-completion key bindings functions

(defun private//private-completion-set-RET-key-behavior (package)
    "Bind RET key appropriately for the given PACKAGE and value of
    `private-completion-return-key-behavior'."
    (cond
        (
            (eq 'company package)
            (let 
                ((map company-active-map))
                (cond
                    ((eq 'complete private-completion-return-key-behavior)
                    (define-key map [return] 'company-complete-selection)
                    (define-key map (kbd "RET") 'company-complete-selection))
                    (t
                    (define-key map [return] 'nil)
                    (define-key map (kbd "RET") 'nil))
                )
            )
        )
        (t (message "Not yet implemented for package %S" package))
    )
)

(defun private//private-completion-set-TAB-key-behavior (package)
    "Bind TAB key appropriately for the given PACKAGE and value of
    `auto-completion-tab-key-behavior'."
    (cond
        (
            (eq 'company package)
            (let ((map company-active-map))
                (cond
                ((eq 'complete private-completion-tab-key-behavior)
                (define-key map (kbd "TAB") 'company-complete-selection)
                (define-key map (kbd "<tab>") 'company-complete-selection))

                ((eq 'cycle private-completion-tab-key-behavior)
                (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
                (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
                (define-key map (kbd "<S-tab>") 'private//company-complete-common-or-cycle-backward)
                (define-key map (kbd "<backtab>") 'private//company-complete-common-or-cycle-backward))
                (t
                (define-key map (kbd "TAB") nil)
                (define-key map (kbd "<tab>") nil)))
            )
        )
        (t (message "Not yet implemented for package %S" package))
    )
)

;; key sequence to complete selection

(defvar private--private-completion-time nil)
(defvar private--private-completion-shadowed-insert-binding nil)
(defvar private--private-completion-shadowed-emacs-binding nil)
(defvar private--private-completion-shadowed-hybrid-binding nil)


;; Editing style

(defun private//company-active-navigation (style)
    "Set navigation for the given editing STYLE."
    (cond
        (
            (or (eq 'vim style) (and (eq 'hybrid style) hybrid-mode-enable-hjkl-bindings))
            (let ((map company-active-map))
                (define-key map (kbd "C-j") 'company-select-next)
                (define-key map (kbd "C-k") 'company-select-previous)
                (define-key map (kbd "C-l") 'company-complete-selection)
            )
            (when (require 'company-quickhelp nil 'noerror)
                (evil-define-key 'insert company-quickhelp-mode-map (kbd "C-k") 'company-select-previous)
            )
        )
        (t
            (let ((map company-active-map))
            (define-key map (kbd "C-n") 'company-select-next)
            (define-key map (kbd "C-p") 'company-select-previous)
            (define-key map (kbd "C-f") 'company-complete-selection))
        )
    )
)


;; Transformers


(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))



;; Yasnippet

(defun private/load-yasnippet ()
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1)
)

(defun private/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t)
)


;; Yasnippet and Smartparens

;; If enabled, smartparens will mess snippets expanded by `hippie-expand`.
;; We want to temporarily disable Smartparens during the snippet expansion and
;; switch it back to the initial state when done.
;;
;; However, there is an asymmetry in Yasnippet's hooks:
;; * `yas-before-expand-snippet-hook' is called for all snippet expansions,
;; including the nested ones.
;; * `yas-after-exit-snippet-hook' is called only for the top level snippet,
;; but NOT for the nested ones.
;;
;; That's why we introduce `spacemacs--yasnippet-expanding' below.



(defun private//company-backend-with-yas (backend)
    (if (or (not private-completion-company-enable-yas)
        (and (listp backend) (member 'company-yasnippet backend)))
        backend
        (append (if (consp backend) backend (list backend)) '(:with company-yasnippet))
    )
)


(defun private//smartparens-disable-before-expand-snippet ()
    "Handler for `yas-before-expand-snippet-hook'.
    Disable smartparens and remember its initial state."
    ;; Remember the initial smartparens state only once, when expanding a top-level snippet.
    (unless private--yasnippet-expanding
        (setq private--yasnippet-expanding t
            private--smartparens-enabled-initially smartparens-mode)
    )
    (smartparens-mode -1)
)

(defun private//smartparens-restore-after-exit-snippet ()
    "Handler for `yas-after-exit-snippet-hook'.
    Restore the initial state of smartparens."
    (setq private--yasnippet-expanding nil)
    (when private--smartparens-enabled-initially
        (smartparens-mode 1)
    )
)