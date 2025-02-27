;; cccatt-mode.el -- CCCATT major emacs mode

(defvar cccatt-font-lock-keywords
 '(
   ("#.*" . 'font-lock-comment-face)
   ("\\<\\(let\\|coh\\|ncoh\\)\\>\\|:\\|=\\|→\\|->\\|⇒\\|=>\\|×\\|*\\|\\." . font-lock-keyword-face)
   ;; ("\\<\\(Hom\\|Type\\)\\>\\|->" . font-lock-builtin-face)
   ;; ("\\<\\(\\)\\>" . font-lock-constant-face)
   ("\\<let[ \t]+\\([^ (=]*\\)" 1 'font-lock-function-name-face)
   ("\\<[n]?coh[ \t]+\\([^ (=]*\\)" 1 'font-lock-function-name-face)
  )
)

(defvar cccatt-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Allow some extra characters in words
    (modify-syntax-entry ?_ "w" st)
    ;; Comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for CCCaTT major mode.")

(defvar cccatt-tab-width 4)

(define-derived-mode cccatt-mode fundamental-mode
  "CCCaTT" "Major mode for CCCaTT files."
  :syntax-table cccatt-mode-syntax-table
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(cccatt-font-lock-keywords))
  (setq mode-name "CCCaTT")
)

(provide 'cccatt-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cccatt\\'" . cccatt-mode))
