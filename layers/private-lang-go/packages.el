;;; packages.el --- private-lang-go layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: 汪旻翔 <wangminxiang@wangminangdeMBP.lan>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; Code:

(setq private-lang-go-packages
  '(
    go-mode
    go-eldoc
    go-guru
    go-rename
    ;; company
    company-go
    ;;flycheck
  )
)

;;(defun private-lang-go/post-init-company ()
;;    (spacemacs|add-company-hook go-mode)
;;)

(defun private-lang-go/init-company-go ()
    (use-package company-go
        :defer t
        :init
        (progn
            (setq company-go-show-annotation t)
            ;;(push 'company-go company-backends-go-mode)
            (add-hook 'go-mode-hook 
                (lambda ()
                    (cl-pushnew (private//company-backend-with-yas 'company-go) company-backends)
                    (company-mode)
                )
            )
            ;;(cl-pushnew (private//company-backend-with-yas 'company-go) company-backends)
        )
    )
)

(defun private-lang-go/init-go-mode()
    (when (memq window-system '(mac ns x))
        (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
            (unless (getenv var)
                (exec-path-from-shell-copy-env var)
            )
        )
    )

    (use-package go-mode
        :defer t
        :init
        (progn
            (add-hook 'go-mode-hook 'private//go-set-tab-width)
        )
        :config
        (progn
            (add-hook 'before-save-hook 'gofmt-before-save)
            (spacemacs/declare-prefix-for-mode 'go-mode "me" "playground")
            (spacemacs/declare-prefix-for-mode 'go-mode "mg" "goto")
            (spacemacs/declare-prefix-for-mode 'go-mode "mh" "help")
            (spacemacs/declare-prefix-for-mode 'go-mode "mi" "imports")
            (spacemacs/declare-prefix-for-mode 'go-mode "mt" "test")
            (spacemacs/declare-prefix-for-mode 'go-mode "mx" "execute")
            (spacemacs/set-leader-keys-for-major-mode 'go-mode
                "hh" 'godoc-at-point
                "ig" 'go-goto-imports
                "ia" 'go-import-add
                "ir" 'go-remove-unused-imports
                "eb" 'go-play-buffer
                "er" 'go-play-region
                "ed" 'go-download-play
                "xx" 'private//go-run-main
                "ga" 'ff-find-other-file
                "gc" 'go-coverage
                "tt" 'private//go-run-test-current-function
                "ts" 'private//go-run-test-current-suite
                "tp" 'private//go-run-package-tests
                "tP" 'private//go-run-package-tests-nested
            )


        )

    )
)

(defun private-lang-go/init-go-eldoc()
    (use-package go-mode
        :defer t
        :init
        (progn
            (add-hook 'go-mode-hook 'go-eldoc-setup)
        )
    )
)

(defun private-lang-go/init-go-guru()
    (spacemacs/declare-prefix-for-mode 'go-mode "mf" "guru")
    (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "fd" 'go-guru-describe
        "ff" 'go-guru-freevars
        "fi" 'go-guru-implements
        "fc" 'go-guru-peers
        "fr" 'go-guru-referrers
        "fj" 'go-guru-definition
        "fp" 'go-guru-pointsto
        "fs" 'go-guru-callstack
        "fe" 'go-guru-whicherrs
        "f<" 'go-guru-callers
        "f>" 'go-guru-callees
        "fo" 'go-guru-set-scope
    )
)

(defun private-lang-go/init-go-rename()
    (use-package go-rename
        :init
        (spacemacs/declare-prefix-for-mode 'go-mode "mr" "rename")
        (spacemacs/set-leader-keys-for-major-mode 'go-mode "rn" 'go-rename)
    )
)
