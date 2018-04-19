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
        (message "Enabled private-completion (using %S)."
            private-completion-front-end)
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

(defun spacemacs//private-completion-set-RET-key-behavior (package)
    "Bind RET key appropriately for the given PACKAGE and value of
    `private-completion-return-key-behavior'."
    (cond
        (
            (eq 'company package)
            (let ((map company-active-map))
                (cond
                    (
                        (eq 'complete private-completion-return-key-behavior)
                        (define-key map [return] 'company-complete-selection)
                        (define-key map (kbd "RET") 'company-complete-selection)
                    )
                    (t
                        (define-key map [return] 'nil)
                        (define-key map (kbd "RET") 'nil)
                    )
                )
            )
        )
        (t (message "Not yet implemented for package %S" package))
    )
)


(defun spacemacs//private-completion-set-TAB-key-behavior (package)
    "Bind TAB key appropriately for the given PACKAGE and value of
    `private-completion-tab-key-behavior'."
    (cond
        (
            (eq 'company package)
            (let ((map company-active-map))
                (cond
                    (
                        (eq 'complete private-completion-tab-key-behavior)
                        (define-key map (kbd "TAB") 'company-complete-selection)
                        (define-key map (kbd "<tab>") 'company-complete-selection)
                    )
                    (
                        (eq 'cycle private-completion-tab-key-behavior)
                        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
                        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
                        (define-key map (kbd "<S-tab>")
                        'spacemacs//company-complete-common-or-cycle-backward)
                        (define-key map (kbd "<backtab>")
                        'spacemacs//company-complete-common-or-cycle-backward)
                    )
                    (t
                        (define-key map (kbd "TAB") nil)
                        (define-key map (kbd "<tab>") nil)
                    )
                )
            )
        )
        (t (message "Not yet implemented for package %S" package))
    )
)


;; Editing style

(defun spacemacs//company-active-navigation (style)
    "Set navigation for the given editing STYLE."
    (cond
        (
            (or (eq 'vim style) (and (eq 'hybrid style) hybrid-mode-enable-hjkl-bindings))
            (let 
                ((map company-active-map))
                (define-key map (kbd "C-j") 'company-select-next)
                (define-key map (kbd "C-k") 'company-select-previous)
                (define-key map (kbd "C-l") 'company-complete-selection)
            )
            (when (require 'company-quickhelp nil 'noerror)
                (evil-define-key 'insert company-quickhelp-mode-map (kbd "C-k") 'company-select-previous)
            )
        )
        (t
            (let 
                ((map company-active-map))
                (define-key map (kbd "C-n") 'company-select-next)
                (define-key map (kbd "C-p") 'company-select-previous)
                (define-key map (kbd "C-f") 'company-complete-selection)
            )
        )
    )
)


;; Transformers

(defun spacemacs//company-transformer-cancel (candidates)
    "Cancel completion if prefix is in the list
    `company-mode-completion-cancel-keywords'"
    (unless (member company-prefix company-mode-completion-cancel-keywords) candidates)
)


(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode) 
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))
    )
)

(defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1))
)

;; Yasnippet
(defun spacemacs/load-yasnippet ()
    (unless yas-global-mode (yas-global-mode 1))
    (yas-minor-mode 1)
)

(defun spacemacs/force-yasnippet-off ()
    (yas-minor-mode -1)
    (setq yas-dont-activate t)
)


(defun spacemacs//company-backend-with-yas (backend)
    (if (or (not private-completion-company-enable-yas)
        (and (listp backend) (member 'company-yasnippet backend)))
        backend
        (append (if (consp backend) backend (list backend)) '(:with company-yasnippet))
    )
)