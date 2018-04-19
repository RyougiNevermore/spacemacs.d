;;; config.el --- private-completion layer config file for Spacemacs.
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

(defvar-local private-completion-front-end 'company
  "Which private-completion front end to use.")

(defvar private-completion-return-key-behavior 'complete
  "What the RET key should do when private-completion menu is active.
Possible values are `complete' or `nil'.")

(defvar private-completion-tab-key-behavior 'cycle
  "What the TAB key should do when private-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar private-completion-enable-help-tooltip `manual
  "If non nil the docstring appears in a tooltip.
If set to `manual', help tooltip appears only when invoked
manually.")

(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

(defvar private-completion-private-snippets-directory nil
  "Configurable private snippets directory.")

(defvar private-completion-company-enable-yas t 
    "Enable yasnippet for all backends.")