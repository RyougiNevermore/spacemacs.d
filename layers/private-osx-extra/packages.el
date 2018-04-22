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


(setq private-osx-extra-packages
  '(
  )
)

(when (spacemacs/system-is-mac)
    (global-set-key (kbd "s-d") 'kill-whole-line)
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
)
