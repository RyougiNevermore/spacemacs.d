;;; config.el --- private-lang-go layer config file for Spacemacs.
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

(spacemacs|defvar-company-backends go-mode)

(spacemacs|define-jump-handlers go-mode godef-jump)

(defvar go-use-gocheck-for-testing nil
    "If using gocheck for testing when running the tests -check.f will be used instead of -run to specify the test that will be ran. Gocheck is mandatory for testing suites.")

(defvar go-tab-width 8
    "Set the `tab-width' in Go mode. Default is 8.")

(defvar go-use-gometalinter nil
    "Use gometalinter if the variable has non-nil value.")