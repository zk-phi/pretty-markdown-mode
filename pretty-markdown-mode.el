;;; pretty-markdown.el --- Fancy semi-WYSIWYG major mode for markdown documents

;; Copyright (C) 2018- zk_phi

(require 'font-lock)
(require 'jit-lock)
(require 'iimage)

(defconst pretty-markdown-version "0.1.0")

(defgroup pretty-markdown nil
  "Fancy semi-WYSIWYG major mode for markdown documents."
  :group 'emacs)

(defcustom pretty-markdown-line-spacing 0.3
  "Line spacing for pretty-markdown mode buffers."
  :group 'pretty-markdown
  :type 'number)

(defcustom pretty-markdown-cursor-type 'bar
  "Cursor-type for pretty-markdown mode buffers"
  :group 'pretty-markdown
  :type 'symbol)

(defcustom pretty-markdown-codeblock-background
  (if (eq frame-background-mode 'light) "#eee" "#111")
  "Background color applied to codeblocks in pretty-markdown mode."
  :group 'pretty-markdown
  :type 'string)

(defface pretty-markdown-default-face
  '((((background light))
     (:family "Times New Roman" :width semi-condensed :height 1.2
              :background "#fff" :foreground "#333"))
    (t
     (:family "Times New Roman" :width semi-condensed :height 1.2
              :background "#333" :foreground "#fff")))
  "Face used as the default face in pretty-markdown buffers."
  :group 'pretty-markdown)

(defface pretty-markdown-h1-face
  '((((background light)) (:height 2.0 :underline "#ccc" :bold t))
    (t (:height 2.0 :underline "#444" :bold t)))
  "Face used to highlight level-1 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-h2-face
  '((t (:height 1.7 :bold t)))
  "Face used to highlight level-2 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-h3-face
  '((t (:height 1.4 :bold t)))
  "Face used to highlight level-3 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-h4-face
  '((t (:height 1.3 :bold t)))
  "Face used to highlight level-4 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-h5-face
  '((t (:height 1.2 :bold t)))
  "Face used to highlight level-5 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-h6-face
  '((t (:height 1.1 :bold t)))
  "Face used to highlight level-6 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-hide-face
  '((t (:height 0.1)))
  "Face used to hide some texts."
  :group 'pretty-markdown)

(defface pretty-markdown-kbd-face
  '((((background light)) (:family "Monospace" :background "#fbf1d4" :box "#ccc"))
    (t (:family "Monospace" :background "#001e26" :box "#444")))
  "Face used to highlight kbd spans."
  :group 'pretty-markdown)

(defface pretty-markdown-italic-bold-face
  '((t (:slant italic :bold t)))
  "Face used to highlight italic and bold spans."
  :group 'pretty-markdown)

(defface pretty-markdown-bold-face
  '((t (:bold t)))
  "Face used to highlight bold spans."
  :group 'pretty-markdown)

(defface pretty-markdown-italic-face
  '((t (:slant italic)))
  "Gace used to highlight italic spans."
  :group 'pretty-markdown)

(defface pretty-markdown-strikethrough-face
  '((t (:strike-through t)))
  "Gace used to highlight italic spans."
  :group 'pretty-markdown)

(defconst pretty-markdown-font-lock-keywords
  '(
    ;; headings
    ("^\\(######[\s\t]+\\)\\(.*\\)$"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-h6-face))
    ("^\\(#####[\s\t]+\\)\\(.*\\)$"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-h5-face))
    ("^\\(####[\s\t]+\\)\\(.*\\)$"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-h4-face))
    ("^\\(###[\s\t]+\\)\\(.*\\)$"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-h3-face))
    ("^\\(##[\s\t]+\\)\\(.*\\)$"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-h2-face))
    ("^\\(#[\s\t]+\\)\\(.*\\)$"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-h1-face))
    ("\\(`\\)\\([^`\n]+\\)\\(`\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-kbd-face)
     (3 'pretty-markdown-hide-face))
    ("\\(\\*\\*\\*\\)\\([^\\*\n]+\\)\\(\\*\\*\\*\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-italic-bold-face)
     (3 'pretty-markdown-hide-face))
    ("\\(\\*\\*\\)\\([^\\*\n]+\\)\\(\\*\\*\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-bold-face)
     (3 'pretty-markdown-hide-face))
    ("\\(\\*\\)\\([^\\*\n]+\\)\\(\\*\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-italic-face)
     (3 'pretty-markdown-hide-face))
    ("\\(~~\\)\\([^\\*\n]+\\)\\(~~\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-strikethrough-face)
     (3 'pretty-markdown-hide-face)))
  "Font lock keywords for pretty-markdown mode.")

(defun pretty-markdown-jit-codeblock-highlighter (b e)
  "Codeblock highlighter for pretty-markdown mode."
  (dolist (ov (overlays-in b e))
    (when (eq (overlay-get ov 'category) 'pretty-markdown-codeblock)
      (delete-overlay ov)))
  (goto-char b)
  (while (search-forward-regexp "^\\(```\\)\\(.+\\)?$" nil e)
    (when (or (match-beginning 2)
              (search-backward-regexp "^\\(```\\)\\(.+\\)$" nil t))
      (let ((bq-beg (match-beginning 1))
            (bq-end (match-end 1))
            (lang-beg (match-beginning 2))
            (lang-end (match-end 2))
            (code-beg (1+ (match-end 0))))
        (when (search-forward-regexp "^\\(```\\)$" nil t)
          (let* ((bq2-beg (match-beginning 1))
                 (bq2-end (match-end 1))
                 (code-end (1- (match-beginning 0)))
                 (mode (intern (concat (buffer-substring lang-beg lang-end) "-mode")))
                 (ov1 (make-overlay bq-beg (1+ bq2-end)))
                 (ov2 (make-overlay code-beg code-end)))
            (put-text-property bq-beg bq-end 'face 'pretty-markdown-hide-face)
            (put-text-property bq2-beg bq2-end 'face 'pretty-markdown-hide-face)
            (put-text-property lang-beg lang-end 'face 'pretty-markdown-kbd-face)
            (overlay-put ov1 'face `(:background ,pretty-markdown-codeblock-background))
            (overlay-put ov2 'face '(:family "Monospace"))
            (overlay-put ov1 'category 'pretty-markdown-codeblock)
            (overlay-put ov2 'category 'pretty-markdown-codeblock)
            (when (fboundp mode)
              ;; based on `org-src-font-lock-fontify-block' in `org-src.el'
              (let ((orig-buf (current-buffer))
                    (orig-str (buffer-substring code-beg code-end))
                    (tmpbuf (get-buffer-create
                             (concat " *pretty-markdown-highlight-" (symbol-name mode))))
                    last next)
                (with-current-buffer tmpbuf
                  (delete-region (point-min) (point-max))
                  (insert orig-str " ")
                  (unless (eq major-mode mode) (funcall mode))
                  (font-lock-fontify-buffer)
                  (setq last (point-min))
                  (while (setq next (next-single-property-change last 'face))
                    (put-text-property (+ code-beg (1- last)) (+ code-beg (1- next))
                                       'face (get-text-property last 'face)
                                       orig-buf)
                    (setq last next)))
                (add-text-properties
                 code-beg code-end
                 '(font-lock-fontified t fontified t font-lock-multiline t))))))))))

(defun pretty-markdown-indent-line ()
  (interactive)
  (when (string-match "^[\s\t]*$" (buffer-substring (point-at-bol) (point-at-eol)))
    (indent-line-to
     (or (save-excursion (and (zerop (forward-line -1)) (skip-chars-forward "\s\t")))
         0))))

(define-derived-mode pretty-markdown-mode text-mode "*Markdown*"
  "Fancy semi-WYSIWYG major mode for markdown documents."
  :group 'pretty-markdown
  (set (make-local-variable 'indent-line-function) 'pretty-markdown-indent-line)
  (set (make-local-variable 'line-spacing) pretty-markdown-line-spacing)
  (set (make-local-variable 'cursor-type) pretty-markdown-cursor-type)
  (set (make-local-variable 'font-lock-defaults) '(pretty-markdown-font-lock-keywords))
  (face-remap-add-relative 'default 'pretty-markdown-default-face)
  (toggle-truncate-lines -1)
  (jit-lock-register 'pretty-markdown-jit-codeblock-highlighter)
  (jit-lock-mode 1)
  ;; comment-start, comment-end, comment-use-syntax, comment-start-skip
  ;; imenu-generic-expression
  )
