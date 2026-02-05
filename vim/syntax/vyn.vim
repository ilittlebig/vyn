if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword vynKeyword local function if else while return break continue true false nil

" Types
syn keyword vynType int string bool double

" Numbers
syn match vynNumber /\<\d\+\(\.\d\+\)\?\>/

" Strings
syn region vynString start=/"/ skip=/\\"/ end=/"/
syn region vynString start=/'/ skip=/\\'/ end=/'/

" Comments
syn match  vynLineComment  /\/\/.*/
syn region vynBlockComment start=/\/\*/ end=/\*\//

" Operators
syn match vynOperator /==\|!=\|<=\|>=\|&&\|||\|[+\-*/%<>=!]/

" Braces/parens punctuation
syn match vynDelimiter /[(){};,:\[\]]/

hi def link vynKeyword      Keyword
hi def link vynType         Type
hi def link vynNumber       Number
hi def link vynString       String
hi def link vynLineComment  Comment
hi def link vynBlockComment Comment
hi def link vynOperator     Operator
hi def link vynDelimiter    Delimiter

let b:current_syntax = "vyn"
