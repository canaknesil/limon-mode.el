
;;
;; UTILITY
;;

(defun set-char-table-range-list (range-list table)
  (mapcar (lambda (range)
	    (set-char-table-range table
				  (car range)
				  (string-to-syntax (cdr range))))
	  range-list))

(defun last-char-before-whitespace-helper ()
  (if (bobp) nil
    (let ((c (char-after)))
      (if (or (string= (char-to-string (char-syntax c)) " ")
		        (char-equal c ?\n))
          (progn
            (backward-char)
            (last-char-before-whitespace-helper))
        c))))
      

(defun last-char-before-whitespace ()
  (save-excursion
    (last-char-before-whitespace-helper)))

  
;;
;; SYNTAX TABLE
;;

(defconst limon-mode-syntax-table-range-list
  '(;; White character
    (?\s . "-") ;;space
    (?\t . "-") ;;tab
    ;;(?\n . "-") ;; Used as comment, look below.

    ;; Word constituents
    ((?a . ?z) . "w")
    ((?A . ?Z) . "w")
    ((?0 . ?9) . "w")
    (?@ . "w")

    ;; Symbol constituents
    (?_ . "_")
    (?. . "_")

    ;; Ponctuation
    (?= . ".")
    (?< . ".")
    (?> . ".")
    (?! . ".")
    (?+ . ".")
    (?- . ".")
    (?* . ". 23") ;; In addition to being in class _, the second
		  ;; character of a two-character comment-start
		  ;; sequence (2), the first character of such a
		  ;; comment-end sequence (3).
    
    (?/ . ". 124b") ;; In addition to being in class ., the start of a
                    ;; two-character comment-start sequence (1), the
		    ;; second character of such a sequence (2), the
		    ;; second character of a two-character comment-end
		    ;; sequence (4) where the comment sequence if of
		    ;; style "b" (b).
    (?% . ".")
    (?& . ".")
    (?| . ".")
    (?? . ".")
    (?: . ".")
    (?# . ".")

    ;; Open delimiter character
    (?\( . "(")
    (?\[ . "(")
    (?\{ . "(")

    ;; Close delimiter character
    (?\) . ")")
    (?\] . ")")
    (?\} . ")")

    ;; String quote character
    (?\" . "\"")
    (?' . "\"")
    
    ;; Escape character
    (?\\ . "\\")

    ;; Character quote character
    
    ;; Paired delimiter

    ;; Expression prefix

    ;; Comment starter
    ;; (?* . "_ 23") ;; Above
    ;; (?/ . "_ 124b") 
    
    ;; Comment ender
    (?\n . "> b") ;; Comment-end character of comment style "b".
    
    ;; Generic comment delimiter

    ;; Generic string delimiter

    ;; Inherit from `standard-syntax-table'

    ))

    
(defvar limon-mode-syntax-table
  (make-char-table 'syntax-table nil)) ;; Empty syntax table

(set-char-table-range-list limon-mode-syntax-table-range-list
			   limon-mode-syntax-table)
			   


;;
;; SYNTAX HIGHLIGHTING
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
     "__array_set__"
     "__string_get__"
     "__string_set__")
   'symbols))

(defconst limon-warning
  (regexp-opt
   '("error")
   'symbols))

(defconst limon-keychar
  (regexp-opt
   '("?"
     "#")
   t)) ;;group

(defconst limon-function-name
  (regexp-opt '("@") t)) ;;group


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
   t)) ;;group

(defconst limon-negation-char
  (regexp-opt
   '("!")
   t)) ;;group

(defconst limon-bracket
  (regexp-opt
   '("[" "]")
   t)) ;;group

(defconst limon-constant-word
  (regexp-opt
   '("true"
     "false"
     "null")
   'symbols))


(defconst limon-constant-int
  "\\_<\\([0-9]+\\|0b[01]+\\|0x[0-9A-Fa-f]+\\)\\_>") ;;symbols

(defconst limon-constant-float
  "\\_<\\([0-9]*\\.[0-9]+\\([pP][1-9][0-9]*\\)?\\)\\_>") ;;symbols

(defconst limon-constant-float-bin
  "\\_<\\(0b[01]*\\.[01]+\\([pP][1-9][0-9]*\\)?\\)\\_>") ;;symbols

(defconst limon-constant-float-hex
  "\\_<\\(0x[0-9A-Fa-f]*\\.[0-9A-Fa-f]+\\([pP][1-9][0-9]*\\)?\\)\\_>") ;;symbols

(defconst limon-symbol
  ":\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)\\_>")

(defconst limon-colon ;; the remaining colons are keychar
  ":")

(defconst limon-font-lock-keyword-variable-name 
  (cons "\\_<\\(def\\)\\>\\s-\\_<\\([_a-zA-Z][_a-zA-Z0-9]+\\)\\_>"
        (list 2 'font-lock-variable-name-face t)))
;; Meaning, 2nd sub expression in regexp, t: override prev highligh



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
   (cons limon-colon 'font-lock-keyword-face)
   limon-font-lock-keyword-variable-name
   (cons limon-negation-char 'font-lock-negation-char-face))
  "Highlighting expressions for Limon mode")


;;
;; INDENTATION
;;

(defvar limon-tab-width 3)

;; Rules (wrt. the beginning of the current line):
;;
;; 1 - If the beginning of the buffer, indent to 0.
;;
;; 2 - If the token just before the point is an openning parenthesis,
;; indent one tab forward from the first symbol after the last
;; unmatched opening parenthesis, or from the beginning of line, if
;; does not exist.
;;
;; 3 - If the token just before the line is NOT an openning
;; parenthesis, indent same as the first "word" just after the last
;; unmatched openning parenthesis, or indent to 0, if does not exist.
;;
;; 4 - If the first token of the line is a closing paranthesis, indent
;; same as the first "word" just after the last unmatched openning
;; paranthesis, except the match of the current one, or indent to 0,
;; if does not exist.

(defun limon-indent-line ()
  (interactive)
  (let ((new-col (current-column))) ;; will be set
    (save-excursion
      (beginning-of-line) ;; Set point to beginning of line.
    
      (cond
     
       ;; RULE 1: Beginning of buffer, indent to 0.
       ((bobp) 
        (indent-line-to 0)
        (setq new-col (current-indentation)))
     
       ;; RULE 2
       ((let ((prev-char (last-char-before-whitespace))) ;; Last char before whitespace.
	       (or (string= (char-to-string (char-syntax prev-char)) "(") ;; is an openning delimiter
              (char-equal prev-char ?#))) ;; [# behaves like openning delimiter
        (let ((base-col
	            (save-excursion
	              (backward-up-list)
	              (if (eq (ignore-errors
			                  (backward-up-list)
                           t) nil) ;; No matching paren
		               0
		             (progn
		               (down-list) (forward-sexp)
		               (backward-sexp) (current-column))))))
	       (indent-line-to (+ limon-tab-width base-col))
          (setq new-col (current-indentation))))

       ;; RULE 4
       ((let ((first-char
               (progn
                 (back-to-indentation) ;; move point to first non-whitespace char
                 (char-after))))
          (string= (char-to-string (char-syntax first-char)) ")"))
        (let ((base-col
	            (save-excursion
                 (backward-char)
	              (backward-up-list)
	              (if (eq (ignore-errors
			                  (backward-up-list)
                           t) nil) ;; No matching paren
		               0
		             (progn
		               (down-list) (forward-sexp)
		               (backward-sexp) (current-column))))))
	       (indent-line-to base-col)
          (setq new-col (current-indentation))))

       ;; RULE 3
       (t
        (let ((base-col
	            (save-excursion
	              (if (eq (ignore-errors
			                  (backward-up-list)
                           t) nil) ;; No matching paren
		               0
		             (progn
		               (down-list) (forward-sexp)
		               (backward-sexp) (current-column))))))
	       (indent-line-to base-col)
          (setq new-col (current-indentation))))
       
       ;; Default, indent as previous line.
       (t
        (let ((prev-line-indent
	            (progn
	              (forward-line -1) ;; move up
	              (current-indentation))))
	       (forward-line 1) ;; move down
	       (indent-line-to prev-line-indent)
          (setq new-col (current-indentation))))))

    ;; Outside save-excursion
        
    (cond
     ;; Indent cursor if blank line
     ((save-excursion
        (beginning-of-line)
        (looking-at "\\s-*\n"))
      (move-to-column new-col t))

     ;; Remove unnecessary spaces if rest of line empty
     ((looking-at "\\s-*\n")
      (just-no-space))

     ;; Move cursor to start of the test if beginning is empty
     ((looking-back "^\\s-*")
      (back-to-indentation))

     ;; Don't change cursor position by default
     (t t))))
          
      


;;
;; OTHER CONFIGURATIONS
;;

;; For comment|uncomment-region function
(setq-default comment-start "//")
(setq-default comment-padding " ")
(setq-default comment-end "") ;; comment ending with newline



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
;; THE ENTRY FUNCTION
;;

(defun limon-mode ()
  "Major mode for editing Limon files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table limon-mode-syntax-table)
  (use-local-map limon-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(limon-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'limon-indent-line)
  (setq major-mode 'limon-mode)
  (setq mode-name "Limon")
  (run-hooks 'limon-mode-hook))

(provide 'limon-mode)
