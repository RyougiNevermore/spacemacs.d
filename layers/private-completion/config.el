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


(defvar private-completion-enable-help-tooltip nil 
  "If non nil the docstring appears in a tooltip. 
  If set to `manual', help tooltip appears only when invoked 
  manually.")

(defvar private-completion-private-snippets-directory nil
  "Configurable private snippets directory.")

(defvar private-completion-company-enable-yas t "Enable yasnippet for all backends.")

(defvar private--smartparens-enabled-initially t
  "Stored whether smartparens is originally enabled or not.")

(defvar private--yasnippet-expanding nil
  "Whether the snippet expansion is in progress.")