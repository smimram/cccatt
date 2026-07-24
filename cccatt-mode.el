;; cccatt-mode.el -- CCCATT major emacs mode  -*- lexical-binding: t; -*-

(require 'compile)

(defgroup cccatt nil
  "Support for the CCCaTT proof assistant."
  :group 'languages)

(defcustom cccatt-command "cccatt"
  "Command used to typecheck CCCaTT files."
  :type 'string
  :group 'cccatt)

(defcustom cccatt-command-options nil
  "Extra command-line options passed to `cccatt-command'."
  :type '(repeat string)
  :group 'cccatt)

(defvar cccatt-output-buffer-name "*cccatt*"
  "Name of the buffer holding the output of the CCCaTT typechecker.")

(defvar cccatt-font-lock-keywords
 '(
   ("#.*" . 'font-lock-comment-face)
   ("\\<\\(let\\|coh\\|ncoh\\|include\\)\\>\\|:\\|=\\|→\\|->\\|⇒\\|=>\\|⇔\\|<=>\\|×\\|*\\|\\." . font-lock-keyword-face)
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

;; Positions are printed by Extlib.Pos.to_string, which produces either
;;   in file F line L character C
;;   in file F line L characters C1-C2
;;   in file F from line L1 character C1 to line L2 character C2
;; Characters are counted from 0, whereas compilation-mode expects columns
;; counted from 1, hence the increments below.
(defvar cccatt-error-regexp-alist
  `(("in file \\(.+?\\) from line \\([0-9]+\\) character \\([0-9]+\\) to line \\([0-9]+\\) character \\([0-9]+\\)"
     1 (2 . 4)
     (,(lambda () (1+ (string-to-number (match-string 3))))
      . ,(lambda () (1+ (string-to-number (match-string 5))))))
    ("in file \\(.+?\\) line \\([0-9]+\\) characters? \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?"
     1 2
     (,(lambda () (1+ (string-to-number (match-string 3))))
      . ,(lambda () (and (match-beginning 4)
                         (1+ (string-to-number (match-string 4))))))))
  "Value for `compilation-error-regexp-alist' in CCCaTT output buffers.")

(define-derived-mode cccatt-output-mode compilation-mode "cccatt-output"
  "Major mode for the output of the CCCaTT typechecker."
  ;; We override the alist instead of adding to it so that the OCaml backtrace
  ;; is not mistaken for a list of errors.
  (setq-local compilation-error-regexp-alist cccatt-error-regexp-alist))

(defun cccatt-check-buffer ()
  "Typecheck the file of the current buffer with CCCaTT.
On success, simply report it in the echo area, otherwise display the
output of the typechecker in a window."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p) (save-buffer))
  (let* ((file buffer-file-name)
         (dir default-directory)
         (buffer (get-buffer-create cccatt-output-buffer-name))
         status)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory dir)
        (setq status (apply #'call-process cccatt-command nil t nil
                            (append cccatt-command-options (list file))))
        ;; Remove the OCaml backtrace following the error message.
        (goto-char (point-min))
        (when (re-search-forward "^\\(?:Raised\\|Re-raised\\|Called from\\)\\_>"
                                 nil t)
          (goto-char (match-beginning 0))
          (skip-chars-backward "\n")
          (delete-region (point) (point-max))
          (insert "\n"))
        (cccatt-output-mode)
        (goto-char (point-min))))
    (if (eq status 0)
        (progn
          (let ((window (get-buffer-window buffer t)))
            (when window (quit-window nil window)))
          (message "CCCaTT typechecking ok"))
      (display-buffer buffer '((display-buffer-reuse-window
                                display-buffer-below-selected)
                               (window-height . 0.3)))
      ;; Jump to the location of the first error, when there is one.
      (with-current-buffer buffer
        (ignore-errors (next-error 1 t)))
      (message "CCCaTT typechecking failed"))))

(defvar cccatt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'cccatt-check-buffer)
    map)
  "Keymap for `cccatt-mode'.")

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
