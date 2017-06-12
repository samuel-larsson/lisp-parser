;;=====================================================================
;; LISP READER & LEXER
;;=====================================================================

;;=====================================================================
;; Help functions
;;=====================================================================
;; ctos         convert a character to a string
;; str_con      concatenate 2 strings str, c
;; whitespace   is c whitespace?
;;=====================================================================

(defun ctos (c)        (make-string 1 :initial-element c))
(defun str-con (str c) (concatenate 'string str (ctos c)))
(defun whitespace (c)  (member c '(#\Space #\Tab #\Newline)))

;;=====================================================================
;; get-wspace   remove whitespace
;;=====================================================================

(defun get-wspace (ip)
   (setf c (read-char ip nil 'EOF))
   (cond
           ((whitespace c)  (get-wspace ip))
           (t                             c)
   )
)

;;=====================================================================
;; Read an Identifier         Compare this with C's do-while construct
;;=====================================================================

(defun get-name (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond
                ((alphanumericp c)  (get-name ip lexeme c))
                (t                  (list        c lexeme))
   )
)

;;=====================================================================
;; Read a Number              Compare this with C's do-while construct
;;=====================================================================

(defun get-number (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond
         ((not (null (digit-char-p c)))  (get-number ip lexeme c))
         (t                              (list          c lexeme))
   )
  )

;;=====================================================================
;; Read a single character or ":="
;;=====================================================================

(defun get-symbol (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c1 c)
   (setf c (read-char ip nil 'EOF))
   (cond
         ((and (char= c1 #\:) (char= c #\=))  (get-symbol ip lexeme c))
         (t                                   (list          c lexeme))
   )
)

;;=====================================================================
;; Read a Lexeme                       lexeme is an "accumulator"
;;                                     Compare this with the C version
;;=====================================================================

(defun get-lex (state)
   (setf lexeme "")
   (setf ip (pstate-stream   state))
   (setf c  (pstate-nextchar state))
   (if (whitespace c) (setf c (get-wspace ip)))
   (cond
         ((eq c 'EOF)                     (list 'EOF ""))
         ((alpha-char-p c)                (get-name   ip lexeme c))
         ((not (null (digit-char-p c)))   (get-number ip lexeme c))
         (t                               (get-symbol ip lexeme c))
   )
)

;;=====================================================================
; map-lexeme(lexeme) returns a list: (token, lexeme)
;;=====================================================================

(defun map-lexeme (lexeme)
(format t "Symbol: ~S ~%" lexeme)
   (list (cond
         ((string=   lexeme "program")  'PROGRAM )
         ((string=   lexeme "input"  )  'INPUT   )
         ((string=   lexeme "output" )  'OUTPUT  )
         ((string=   lexeme "var"    )  'VAR     )
         ((string=   lexeme "integer")  'TYPE    )
         ((string=   lexeme "boolean")  'TYPE    )
         ((string=   lexeme "real"   )  'TYPE    )
         ((string=   lexeme "begin"  )  'BEGIN   )
         ((string=   lexeme "end"    )  'END     )
         ((string=   lexeme "("      )  'LP      )
         ((string=   lexeme ")"      )  'RP      )
         ((string=   lexeme "*"      )  'MULT    )
         ((string=   lexeme "+"      )  'ADDI    )
         ((string=   lexeme ","      )  'COMMA   )
         ((string=   lexeme ";"      )  'SCOLON  )
         ((string=   lexeme ":"      )  'COLON   )
         ((string=   lexeme "="      )  'EQUALS  )
         ((string=   lexeme "."      )  'FSTOP   )
         ((string=   lexeme ":="      )  'ASSIGN )
         ((string=   lexeme ""       )	'EOF     )
         ((is-id     lexeme          )  'ID      )
         ((is-number lexeme          )  'OPERAND )
         (t                             'UNKNOWN )
         )
    lexeme)
)

;;=====================================================================
; ID is [A-Z,a-z][A-Z,a-z,0-9]*          number is [0-9][0-9]*
;;=====================================================================

(defun is-id (str)
  (and (every #'alphanumericp (rest (coerce str 'list))) (alpha-char-p (first (coerce str 'list))))
)

(defun is-number (str)
  (every #'digit-char-p str)
)

;;=====================================================================
; THIS IS THE PARSER PART
;;=====================================================================

;;=====================================================================
; Create a stucture - parse state descriptor
;;=====================================================================
; lookahead is the list (token, lexeme)
; stream    is the input filestream
; nextchar  is the char following the last lexeme read
; status    is the parse status (OK, NOTOK)
; symtab    is the symbol table
;;=====================================================================

(defstruct pstate
    (lookahead)
    (stream)
    (nextchar)
    (status)
    (symtab)
)

;;=====================================================================
; Constructor for the structure pstate - initialise
; stream to the input file stream (ip)
;;=====================================================================

(defun create-parser-state (ip)
   (make-pstate
      :stream        ip
      :lookahead     ()
      :nextchar      #\Space
      :status        'OK
      :symtab        ()
    )
)

;;=====================================================================
; SYMBOL TABLE MANIPULATION
;;=====================================================================

;;=====================================================================
; token  - returns the token  from (token lexeme)(reader)
; lexeme - returns the lexeme from (token lexeme)(reader)
;;=====================================================================

(defun token  (state) (first (pstate-lookahead state))
)
(defun lexeme (state) (second (pstate-lookahead state))
)

;;=====================================================================
; symbol table manipulation: add + lookup + display
;;=====================================================================

(defun symtab-add (state id)
  (cond
    ((equal (pstate-symtab state) nil) (setf (pstate-symtab state) (list id)))
    ((equal (symtab-member state id) nil) (setf (pstate-symtab state) (append (pstate-symtab state) (list id))))
    (t  (semerr1 state))
  )
)

(defun symtab-member (state id)
  (if (member t (mapcar (lambda (s) (string= s id)) (pstate-symtab state))) t)
)

(defun symtab-display (state)
   (format t "------------------------------------------------------~%")
   (format t "Symbol Table is: ~S ~%" (pstate-symtab state))
   (format t "------------------------------------------------------~%")
)

;;=====================================================================
; Error functions: Syntax & Semantic
;;=====================================================================

(defun synerr1 (state symbol)
    (format t "*** Syntax error:   Expected ~8S found ~8S ~%"
           symbol (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr2 (state)
    (format t "*** Syntax error:   Expected TYPE     found ~8S ~%"
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr3 (state)
    (format t "*** Syntax error:   Expected OPERAND  found ~8S ~%"
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr1 (state)
    (format t "*** Semantic error: ~S already declared.~%"
                (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr2 (state)
    (format t "*** Semantic error: ~S not declared.~%"
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr3 (state)
    (format t "*** Semantic error: found ~8S expected EOF.~%"
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

;;=====================================================================
; The return value from get-token is always a list. (token lexeme)
;;=====================================================================

(defun get-token (state)
  (let    ((result (get-lex state)))
    (setf (pstate-nextchar  state) (first result))
    (setf (pstate-lookahead state) (map-lexeme (second result)))
  )
 )

;;=====================================================================
; match compares lookahead with symbol (the expected token)
; if symbol == lookahead token ==> get next token else Syntax error
;;=====================================================================

(defun match (state symbol)
   (if (eq symbol (token state))
       (get-token  state)
       (synerr1    state symbol)
       )
)

;;=====================================================================
; THE GRAMMAR RULES
;;=====================================================================

;;=====================================================================
; <stat-part>     --> begin <stat-list> end .
; <stat-list>     --> <stat> | <stat> ; <stat-list>
; <stat>          --> <assign-stat>
; <assign-stat>   --> id := <expr>
; <expr>          --> <term>     | <term> + <expr>
; <term>          --> <factor>   | <factor> * <term>
; <factor>        --> ( <expr> ) | <operand>
; <operand>       --> id | number
;;=====================================================================

(defun stat-part (state)
  (match state 'BEGIN)
  (stat-list state)
  (match state 'END)
  (match state 'FSTOP)
)

(defun stat-list-aux (state)
  (match state 'SCOLON)
  (stat-list state)
)

(defun stat-list (state)
  (stat state)
  (if (eq (first (pstate-lookahead state)) 'SCOLON) (stat-list-aux state))
)

(defun stat (state)
  (assign-stat state)
)

(defun id-check (state)
(if (and (eq (first (pstate-lookahead state)) 'ID)
      (not (symtab-member state (second (pstate-lookahead state))))) (semerr2 state))
)

(defun assign-stat (state)
  (id-check state)
  (match state 'ID)
  (match state 'ASSIGN)
  (expr state)
)

(defun expr-aux (state)
  (match state 'ADDI)
  (expr state)
)

(defun expr (state)
  (term state)
  (if (eq (first (pstate-lookahead state)) 'ADDI) (expr-aux state))
)

(defun term-aux (state)
  (match state 'MULT)
  (term state)
)

(defun term (state)
  (factor state)
  (if (eq (first (pstate-lookahead state)) 'MULT) (term-aux state))
)

(defun pexpr (state)
  (match state 'LP)
  (expr state)
  (match state 'RP)
)

(defun factor (state)
  (cond
    ((eq (first (pstate-lookahead state)) 'LP) (pexpr state))
    (t (operand state))
  )
)

(defun operand-id (state)
  (if (not (symtab-member state (second (pstate-lookahead state))))
    (semerr2 state)) (match state 'ID)
)

(defun operand (state)
  (cond
    ((eq (first (pstate-lookahead state)) 'ID) (operand-id state))
    (t  (match state 'OPERAND))
  )
)

;;=====================================================================
; <var-part>     --> var <var-dec-list>
; <var-dec-list> --> <var-dec> | <var-dec><var-dec-list>
; <var-dec>      --> <id-list> : <type> ;
; <id-list>      --> id | id , <id-list>
; <type>         --> integer | real | boolean
;;=====================================================================

(defun var-part (state)
  (match state 'VAR)
  (var-dec-list state)
)

(defun var-dec-list (state)
  (var-dec state)
  (if (eq (first (pstate-lookahead state)) 'ID) (var-dec-list state))
)

(defun var-dec (state)
  (id-list state)
  (match state 'COLON)
  (typ state)
  (match state 'SCOLON)
)

(defun check-id (state)
  (if (eq (first (pstate-lookahead state)) 'ID) (symtab-add state (second (pstate-lookahead state))))
)

(defun id-list-aux (state)
  (match state 'COMMA)
  (id-list state)
)

(defun id-list (state)
  (check-id state)
  (match state 'ID)
  (if (eq (first (pstate-lookahead state)) 'COMMA) (id-list-aux state))
)

(defun typ (state)
  (cond
    ((eq (first (pstate-lookahead state)) 'INTEGER) (match state 'TYPE))
    ((eq (first (pstate-lookahead state)) 'REAL) (match state 'TYPE))
    (t (match state 'TYPE))
  )
)

;;=====================================================================
; <program-header>
;;=====================================================================

(defun program-header (state)
  (match state 'PROGRAM )
  (match state 'ID      )
  (match state 'LP    )
  (match state 'INPUT   )
  (match state 'COMMA   )
  (match state 'OUTPUT  )
  (match state 'RP    )
  (match state 'SCOLON  )

)

;;=====================================================================
; <program> --> <program-header><var-part><stat-part>
;;=====================================================================
(defun program (state)
   (program-header state)
   (var-part       state)
   (stat-part      state)
)

;;=====================================================================
; THE PARSER - parse a file
;;=====================================================================

(defun check-end (state)
  (cond
    ((not (eq (first (pstate-lookahead state)) 'EOF)) (semerr3 state) (get-token state) (check-end state))
    (t nil)
  )
)



;;=====================================================================
; Test parser for file name input
;;=====================================================================

(defun parse (filename)
   (format t "~%------------------------------------------------------")
   (format t "~%--- Parsing program: ~S " filename)
   (format t "~%------------------------------------------------------~%")
   (with-open-file (ip (open filename) :direction :input)
      (setf state (create-parser-state ip))
      (setf (pstate-nextchar state) (read-char ip nil 'EOF))
      (get-token      state)
      (program        state)
      (check-end      state)
      (symtab-display state)
      )
   (if (eq (pstate-status state) 'OK)
      (format t "Parse Successful. ~%")
      (format t "Parse Fail. ~%")
      )
   (format t "------------------------------------------------------~%")
)

;;=====================================================================
; THE PARSER - parse all the test files
;;=====================================================================

(defun parse-all ()

(mapcar 'parse '(
"testfiles/testa.pas"
"testfiles/testb.pas"
"testfiles/testc.pas"
"testfiles/testd.pas"
"testfiles/teste.pas"
"testfiles/testf.pas"
"testfiles/testg.pas"
"testfiles/testh.pas"
"testfiles/testi.pas"
"testfiles/testj.pas"
"testfiles/testk.pas"
"testfiles/testl.pas"
"testfiles/testm.pas"
"testfiles/testn.pas"
"testfiles/testo.pas"
"testfiles/testp.pas"
"testfiles/testq.pas"
"testfiles/testr.pas"
"testfiles/tests.pas"
"testfiles/testt.pas"
"testfiles/testu.pas"
"testfiles/testv.pas"
"testfiles/testw.pas"
"testfiles/testx.pas"
"testfiles/testy.pas"
"testfiles/testz.pas"
"testfiles/testok1.pas"
"testfiles/testok2.pas"
"testfiles/testok3.pas"
"testfiles/testok4.pas"
"testfiles/testok5.pas"
"testfiles/testok6.pas"
"testfiles/testok7.pas"
"testfiles/fun1.pas"
"testfiles/fun2.pas"
"testfiles/fun3.pas"
"testfiles/fun4.pas"
"testfiles/fun5.pas"
"testfiles/sem1.pas"
"testfiles/sem2.pas"
"testfiles/sem3.pas"
"testfiles/sem4.pas"
"testfiles/sem5.pas"
))
)

(defun parsing-output (state))

;;=====================================================================
; THE PARSER - test all files
;;=====================================================================

(parse-all)

;;=====================================================================
; THE PARSER - test a single file
;;=====================================================================

;;(parse filename)

;;=====================================================================
; THE PARSER - end of code
;;=====================================================================
