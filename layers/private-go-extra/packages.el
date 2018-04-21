;;; packages.el --- private-go-extra layer packages file for Spacemacs.
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

(setq private-go-extra-packages
  '(
    go-dlv
    go-impl
    go-tag
    go-direx
    go-gen-test
  )
)

(defun private-go-extra/init-go-dlv ()
  (use-package go-dlv
    :defer t
    :config 
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mD" "debug")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "Df" 'dlv
      )
    )
  )
)

(defun private-go-extra/init-go-impl ()
  (use-package go-impl
    :defer t
    :config 
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mI" "Implement")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "Ii" 'go-impl
      )
    )
  )
)

(defun private-go-extra/init-go-tag ()
  (use-package go-tag
    :defer t
    :config 
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mT" "tag")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "Ta" 'go-tag-add
        "Tr" 'go-tag-remove
      )
      (define-key go-mode-map (kbd "C-t") 'go-tag-add)
    )
  )
)

(defun private-go-extra/init-go-direx ()
  (use-package go-direx
    :defer t
    :config 
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "md" "direx")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "db" 'go-direx-pop-to-buffer
      )
    )
  )
)

(defun private-go-extra/init-go-gen-test ()
  (use-package go-gen-test
    :defer t
    :config 
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "tg" 'go-gen-test-dwim
      )
    )
  )
)

;;; packages.el ends here
