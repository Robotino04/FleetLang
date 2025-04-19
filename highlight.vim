" syntax groups
syntax keyword fleetKeyword let on sync detach observe attach auto unsync drop
syntax keyword fleetControl while if else return
syntax keyword fleetBuiltin print exit
syntax match   fleetNumber "\v(\d+(\.\d*)?|\.\d+)"

syntax region fleetInlineCode start=/`/ end=/`/ contained contains=fleetKeyword,fleetControl,fleetBuiltin,fleetType,fleetVariable,fleetFunction,fleetNumber,fleetSpecialVar,fleetString

syntax match   fleetComment "//.*$" contains=fleetInlineCode
syntax region  fleetCommentBlock start="/\*" end="\*/" contains=fleetInlineCode

syntax match   fleetType "\<i32\>"
syntax match   fleetType "\<sync\s\+i32\>"
syntax match   fleetVariable "\<[a-zA-Z_][a-zA-Z0-9_]*\>"
syntax match   fleetFunction "\<[a-zA-Z_][a-zA-Z0-9_]*\>\s*("he=e-1 contains=fleetFunctionName 
syntax region  fleetString start=/"/ skip=/\\"/ end=/"/

syntax match fleetSpecialVar "\<self\>\(\.threads\[\d\]\|\.gpus\[\d\]\)\?"

" highlight groups
highlight link fleetKeyword        Keyword
highlight link fleetControl        Conditional
highlight link fleetBuiltin        Function
highlight link fleetType           Type
highlight link fleetVariable       Identifier
highlight link fleetFunction       Function
highlight link fleetComment        Comment
highlight link fleetCommentBlock   Comment
highlight link fleetNumber         Number
highlight link fleetString         String
highlight link fleetSpecialVar     Constant
highlight link fleetInlineCode NONE

