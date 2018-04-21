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


;; Yasnippet

(defun private//load-yasnippet ()
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