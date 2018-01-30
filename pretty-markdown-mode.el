;;; pretty-markdown.el --- Fancy semi-WYSIWYG major mode for markdown documents

;; Copyright (C) 2018- zk_phi

;; TODOs:
;; - ordered list "1. 1. 1." support
;; - link '[description](url "title")'
;; - auto link "http://google.com"
;; - linkref '[description][ref]' and '[ref]: url "title"'
;; - remarks "[^1]" and " [^1]: ..."
;; - inline image '![alt-text](url "title")'
;; - tables with autoformatter (like org-mode)
;; - "```math" support powered by magic-latex-buffer.el
;; - emoji support

;; BUG: Possible infinite loop when marking-down a second code block ?
;;
;; ```c
;; int hogehoge (char hoge) {
;;     return (int)hoge;
;; }
;; ```
;;
;; ``

;; COLORS:
;; dark background: #eee (fg),  #222 (bg) , #333 (brighter bg) , #555 (borders)
;; light background: #222 (fg),  #fff (bg), #eee (darker bg) , #bbb (borders)

(require 'font-lock)
(require 'jit-lock)
(require 'iimage)

(defconst pretty-markdown-version "0.1.0")

;; * configurable variables and faces

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

(defcustom pretty-markdown-disabled-global-minor-modes
  '(global-hl-line-mode)
  "List of global minor modes to try to disable when
pretty-markdown mode is turned on."
  :group 'pretty-markdown
  :type 'string)

(defface pretty-markdown-default-face
  '((((background light))
     (:family "Times New Roman" :width semi-condensed :height 1.2
              :background "#fff" :foreground "#222"))
    (t
     (:family "Times New Roman" :width semi-condensed :height 1.2
              :background "#222" :foreground "#eee")))
  "Face used as the default face in pretty-markdown buffers."
  :group 'pretty-markdown)

(defface pretty-markdown-h1-face
  '((((background light)) (:height 2.0 :underline "#bbb" :bold t))
    (t (:height 2.0 :underline "#555" :bold t)))
  "Face used to highlight level-1 headings."
  :group 'pretty-markdown)

(defface pretty-markdown-h2-face
  '((((background light)) (:height 1.6 :bold t))
    (t (:height 1.6 :bold t)))
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
  '((((background light)) (:foreground "#fff" :height 0.1))
    (t (:foreground "#222" :height 0.1)))
  "Face used to hide some texts."
  :group 'pretty-markdown)

(defface pretty-markdown-codeblock-face
  '((((background light)) (:family "Monospace" :background "#eee"))
    (t (:family "Monospace" :background "#333")))
  "Face used to highlight codeblocks."
  :group 'pretty-markdown)

(defface pretty-markdown-kbd-face
  '((((background light)) (:family "Monospace" :background "#eee" :box "#bbb"))
    (t (:family "Monospace" :background "#333" :box "#555")))
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
  "Face used to highlight italic spans."
  :group 'pretty-markdown)

(defface pretty-markdown-strikethrough-face
  '((t (:strike-through t)))
  "Face used to highlight italic spans."
  :group 'pretty-markdown)

(defface pretty-markdown-hr-face
  '((((background light)) (:background "#bbb" :foreground "#bbb" :height 0.1))
    (t (:background "#555" :foreground "#555" :height 0.1)))
  "Face used to render horizontal rules."
  :group 'pretty-markdown)

(defface pretty-markdown-quote-face
  '((((background light)) (:background "#bbb" :foreground "#bbb" :height 0.5))
    (t (:background "#555" :foreground "#555" :height 0.5)))
  "Face used to render horizontal rules."
  :group 'pretty-markdown)

;; * utility functions

(defun pretty-markdown-remove-pretty-overlays (from to category)
  (let ((category (intern (concat "pretty-markdown-" (symbol-name category)))))
    (dolist (ov (overlays-in from to))
      (when (eq (overlay-get ov 'category) category)
        (delete-overlay ov)))))

(defun pretty-markdown-make-pretty-overlay (from to category &rest props)
  (let* ((ov (make-overlay from to))
         (category (intern (concat "pretty-markdown-" (symbol-name category))))
         (hooks (list `(lambda (&rest _) (delete-overlay ,ov)))))
    (overlay-put ov 'category category)
    (overlay-put ov 'modification-hooks hooks)
    (overlay-put ov 'insert-in-front-hooks hooks)
    (while props
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))
    ov))

;; * font-lock highlighter

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
    ("\\(?:^\\|[^`]\\)\\(`\\)\\(`*[^`\n]+`*\\)\\(`\\)\\(?:$\\|[^`]\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-kbd-face)
     (3 'pretty-markdown-hide-face))
    ("\\(\\*\\*\\*\\|___\\)\\([^\\*\n]+\\)\\(\\*\\*\\*\\|___\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-italic-bold-face)
     (3 'pretty-markdown-hide-face))
    ("\\(\\*\\*\\|__\\)\\([^\\*\n]+\\)\\(\\*\\*\\|__\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-bold-face)
     (3 'pretty-markdown-hide-face))
    ("\\(\\*\\|_\\)\\([^\\*\n]+\\)\\(\\*\\|_\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-italic-face)
     (3 'pretty-markdown-hide-face))
    ("\\(~~\\)\\([^\\*\n]+\\)\\(~~\\)"
     (1 'pretty-markdown-hide-face)
     (2 'pretty-markdown-strikethrough-face)
     (3 'pretty-markdown-hide-face))
    ("^\\(\\*\\*\\(\\*\\)+\\|--\\(-\\)+\\|- -\\( -\\)+\\|\\* \\*\\( \\*\\)+\\)\n"
     . 'pretty-markdown-hr-face)
    ("^[\s\t]*\\(>\\) "
     (1 'pretty-markdown-quote-face)))
  "Font lock keywords for pretty-markdown mode.")

;; * codeblock highlighter

(defun pretty-markdown-jit-codeblock-highlighter (b e)
  "Codeblock highlighter for pretty-markdown mode."
  (dolist (ov (overlays-in b e))
    (when (eq (overlay-get ov 'category) 'pretty-markdown-codeblock)
      (delete-overlay ov)))
  (goto-char b)
  (while (search-forward-regexp "^\\(```\\)\\(\\([^:\n]+\\)\\(:.+\\)?\\)?$" e t)
    (when (or (match-beginning 3)
              (search-backward-regexp "^\\(```\\)\\(\\([^:\n]+\\)\\(:.+\\)?\\)$" nil t))
      (let ((bq-beg (match-beginning 1))
            (bq-end (match-end 1))
            (lang-beg (match-beginning 3))
            (lang-end (match-end 3))
            (file-beg (match-beginning 4))
            (file-end (match-end 4))
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
            (when file-beg
              (put-text-property file-beg file-end 'face 'pretty-markdown-kbd-face))
            (overlay-put ov1 'face 'pretty-markdown-codeblock-face)
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

;; * list prettifier

;; (defun pretty-markdown-jit-ordered-list-highlighter (b e)
;;   ;; jump back to the beginning of the first list
;;   (goto-char (point-at-bol))
;;   (while (and (looking-at "[\s\t]*\\([0-9]+\\.\\) +") (zerop (forward-line -1))))
;;   ;; apply highlights
;;   (pretty-markdown-remove-pretty-overlays (point) e 'list)
;;   (while (search-forward-regexp "^[\s\t]*\\([0-9]+\\.\\) +" e t)
;;     (print (buffer-substring (point-at-bol) (point-at-eol)))
;;     (let ((cnt 1))
;;       (while (progn
;;                (pretty-markdown-make-pretty-overlay
;;                 (match-beginning 1) (match-end 1) 'olist 'display (format "%d." cnt))
;;                (and (zerop (forward-line 1)) (looking-at "[\s\t]*\\([0-9]+\\.\\) +")))
;;         (setq cnt (1+ cnt))))))

(defun pretty-markdown-jit-unordered-list-highlighter (b e)
  (pretty-markdown-remove-pretty-overlays b e 'list)
  (while (search-forward-regexp "^[\s\t]*\\([-*+]\\) +\\(\\[[x ]\\]\\)?" e t)
    (pretty-markdown-make-pretty-overlay (match-beginning 1) (match-end 1) 'list 'display "・")
    (when (match-beginning 2)
      (pretty-markdown-make-pretty-overlay
       (match-beginning 2) (match-end 2) 'list
       'display (if (string= "[x]" (match-string 2)) "☑︎" "☐")))))

;; * the major mode

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
  (set (make-local-variable 'left-fringe-width) 0)
  (set (make-local-variable 'right-fringe-width) 0)
  (set-window-buffer (selected-window) (current-buffer))
  (run-hooks 'window-configuration-change-hook)
  (face-remap-add-relative 'default 'pretty-markdown-default-face)
  (toggle-truncate-lines -1)
  (jit-lock-register 'pretty-markdown-jit-codeblock-highlighter)
  ;; (jit-lock-register 'pretty-markdown-jit-ordered-list-highlighter)
  (jit-lock-register 'pretty-markdown-jit-unordered-list-highlighter)
  (jit-lock-mode 1)
  (dolist (mode pretty-markdown-disabled-global-minor-modes)
    (when (and (boundp mode) (symbol-value mode))
      (set (make-local-variable mode) nil)))
  (when (fboundp 'rainbow-mode)
    (rainbow-mode 1))
  ;; comment-start, comment-end, comment-use-syntax, comment-start-skip
  ;; imenu-generic-expression
  )
