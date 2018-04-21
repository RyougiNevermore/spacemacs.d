;;; funcs.el --- private-lang-go layer func file for Spacemacs.
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


(defun private//load-gopath-file(gopath name)
    "Search for NAME file in all paths referenced in GOPATH."
    (let*
        (
            (sep (if (spacemacs/system-is-mswindows) ";" ":"))
            (paths (split-string gopath sep))
            found
        )
        (loop for p in paths
            for file = (concat p name) when (file-exists-p file)
            do
            (load-file file)
            (setq found t)
            finally return found
        )
    )
)

(defun private//go-enable-gometalinter ()
    "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
    (setq flycheck-disabled-checkers '(go-gofmt
                                      go-golint
                                      go-vet
                                      go-build
                                      go-test
                                      go-errcheck)
    )
    (flycheck-gometalinter-setup)
)

(defun private//go-set-tab-width ()
    "Set the tab width."
    (setq-local tab-width go-tab-width)
)

(defun private//go-run-tests (args)
    (interactive)
    (save-selected-window
        (async-shell-command (concat "go test " args))
    )
)

(defun private//go-run-package-tests ()
    (interactive)
    (private//go-run-tests "")
)

(defun private//go-run-package-tests-nested ()
    (interactive)
    (private//go-run-tests "./...")
)

(defun private//go-run-test-current-function ()
    (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
        (let
            (
                (test-method (if go-use-gocheck-for-testing "-check.f" "-run"))
            )
            (save-excursion
                (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
                (private//go-run-tests (concat test-method "='" (match-string-no-properties 2) "'"))
            )
        )
        (message "Must be in a _test.go file to run go-run-test-current-function")
    )
)

(defun private//go-run-test-current-suite ()
    (interactive)
    (if (string-match "_test\.go" buffer-file-name)
        (if go-use-gocheck-for-testing
            (save-excursion
                (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
                (private//go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'"))
            )
            (message "Gocheck is needed to test the current suite")
        )
        (message "Must be in a _test.go file to run go-test-current-suite")
    )
)

(defun private//go-run-main ()
    (interactive)
    (shell-command
        (format "go run %s" (shell-quote-argument (buffer-file-name)))
    )
)
