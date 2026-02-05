if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword vynKeyword local function if else while return break continue true false nil

" Types
syn keyword vynType int string bool double

" Numbers
syn match vynNumber    /\<\d\+\(\.\d\+\)\?\>/                       containedin=ALLBUT,vynLineComment,vynBlockComment

" Strings
syn region vynString   start=/"/ skip=/\\"/ end=/"/                 containedin=ALLBUT,vynLineComment,vynBlockComment
syn region vynString   start=/'/ skip=/\\'/ end=/'/                 containedin=ALLBUT,vynLineComment,vynBlockComment

" Comments
syn match  vynLineComment  +//.*$+          contains=@Spell containedin=ALL
syn region vynBlockComment start=+/\*+ end=+\*/+ keepend contains=@Spell containedin=ALL

" Operators
syn match vynOperator  /==\|!=\|<=\|>=\|&&\|\|\|[+\-*/%<>=!]/ containedin=ALLBUT,vynLineComment,vynBlockComment

" Braces/parens punctuation
syn match vynDelimiter /[(){};,:\[\]]/                              containedin=ALLBUT,vynLineComment,vynBlockComment

hi def link vynKeyword      Keyword
hi def link vynType         Type
hi def link vynNumber       Number
hi def link vynString       String
hi def link vynLineComment  Comment
hi def link vynBlockComment Comment
hi def link vynOperator     Operator
hi def link vynDelimiter    Delimiter

let b:current_syntax = "vyn"
