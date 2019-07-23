;;
;; CONSTANT PARAMETERS
;;

(defconst limon-keyword
  (regexp-opt
   '("def"
     "__sizeof__"
     "__2str__"
     "__2char__"
     "__2int__"
     "__2float__"
     "while"
     "run"
     "__valuetype__"
     "__gensym__"
     "__same__"
     "__make_array__"
     "__array_get__"
     "__array_set__")
   'symbols))

(defconst limon-warning
  "error")

(defconst limon-keychar
  (regexp-opt
   '("?"
     "#")
   t))

(defconst limon-function-name
  (regexp-opt '("@") t))


(defconst limon-operator
  (regexp-opt
   '(">="
     "<="
     "=="
     "!="
     "+="
     "-="
     "*="
     "/="
     "%="
     "&="            
     "|="
     "="
     "\\+"
     "-"
     "/"
     "\\*"
     "%"
     "<"
     ">"
     "&"
     "|")
   t))

(defconst limon-negation-char
  "!")

(defconst limon-bracket
  (regexp-opt '("[" "]")))

(defconst limon-constant-word
  (regexp-opt
   '("true"
     "false"
     "null")
   'symbols))

(defconst limon-constant-int
  "\\<\\([0-9]+\\|0b[01]+\\|0x[0-9A-Fa-f]+\\)\\>")

(defconst limon-constant-float
  "\\<[0-9]*\\.[0-9]+\\([pP][1-9][0-9]*\\)?\\>")

(defconst limon-constant-float-bin
  "\\<0b[01]*\\.[01]+\\([pP][1-9][0-9]*\\)?\\>")

(defconst limon-constant-float-hex
  "\\<0x[0-9A-Fa-f]*\\.[0-9A-Fa-f]+\\([pP][1-9][0-9]*\\)?\\>")

(defconst limon-symbol ;; TODO
  "\\<\\(:[A-Za-z_][A-Za-z0-9_]*\\)\\>")

(defconst limon-variable-name
  "\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>")

(defconst limon-colon
  "\\(:\\)")


;;
;; UTILITY
;;


;;
;; SETUP
;;

;; Allow the user to run their own code when Limon mode is run.
(defvar limon-mode-hook nil)

(defvar limon-mode-map
  (let ((map (make-keymap)))
    ;; This key is already set, you can add other keys like this.
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Limon major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lmn\\'" . limon-mode))


;;
;; SYNTAX HIGHLIGHTING
;;

(defconst limon-font-lock-keywords
  (list
   (cons limon-keyword 'font-lock-keyword-face)
   (cons limon-warning 'font-lock-warning-face)
   (cons limon-keychar 'font-lock-keyword-face)
   (cons limon-function-name 'font-lock-function-name-face)
   (cons limon-bracket 'font-lock-function-name-face)
   (cons limon-constant-word 'font-lock-constant-face)
   (cons limon-constant-float 'font-lock-constant-face)
   (cons limon-constant-float-bin 'font-lock-constant-face)
   (cons limon-constant-float-hex 'font-lock-constant-face)
   (cons limon-constant-int 'font-lock-constant-face)
   (cons limon-symbol 'font-lock-type-face)
   (cons limon-variable-name 'font-lock-variable-name-face)
   (cons limon-colon 'font-lock-keyword-face)
   (cons limon-negation-char 'font-lock-negation-char-face))
  "Highlighting expressions for Limon mode")


(defvar limon-mode-syntax-table
  (let ((limon-mode-syntax-table
	 (make-syntax-table))); c-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" limon-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" limon-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" limon-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" limon-mode-syntax-table)
    limon-mode-syntax-table)
  "Syntax table for limon-mode")


;;
;; THE ENTRY FUNCTION
;;

(defun limon-mode ()
  "Major mode for editing Limon files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table limon-mode-syntax-table)
  (use-local-map limon-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(limon-font-lock-keywords))
  ;(set (make-local-variable 'indent-line-function) 'limon-indent-line)
  (setq major-mode 'limon-mode)
  (setq mode-name "Limon")
  (run-hooks 'limon-mode-hook))

(provide 'limon-mode)
