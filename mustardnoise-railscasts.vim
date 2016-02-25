" Name:         mustardnoise-railscasts.vim
" Maintainer:   Eugene Kruglov <mustardnoise>
" Last Change:  18 October 2015

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "mustardnoise-railscasts"

hi link htmlTag                         xmlTag
hi link htmlTagName                     xmlTagName
hi link htmlEndTag                      xmlEndTag

highlight Normal                        ctermfg=254 ctermbg=234

highlight Cursor                        ctermfg=0   ctermbg=15
highlight CursorLine                    ctermbg=237 cterm=NONE
highlight CursorLineNr                  ctermfg=220 cterm=NONE
highlight CursorColumn                  ctermbg=237 cterm=NONE
highlight ColorColumn                   ctermbg=233
highlight! link CursorColumn ColorColumn
highlight NonText                       ctermfg=235

highlight Comment                       ctermfg=180
highlight Constant                      ctermfg=31
highlight Define                        ctermfg=172
highlight Error                         ctermfg=221 ctermbg=88
highlight Function                      ctermfg=221 cterm=NONE
highlight Identifier                    ctermfg=73  cterm=NONE
highlight Include                       ctermfg=172 cterm=NONE
highlight PreCondit                     ctermfg=173 cterm=NONE
highlight Keyword                       ctermfg=172 cterm=NONE
highlight LineNr                        ctermfg=240 ctermbg=233
highlight Number                        ctermfg=107
highlight VertSplit                     ctermfg=236 ctermbg=236
highlight Directory                     ctermfg=107
highlight StatusLine                    ctermfg=236
highlight SignColumn                    ctermbg=233
highlight Pmenu                         ctermfg=230 ctermbg=234
highlight PmenuSel                      ctermfg=232 ctermbg=64
highlight PMenuSbar                     ctermbg=64
highlight PMenuThumb                    ctermbg=240

highlight PreProc                       ctermfg=103
highlight Search                        ctermfg=NONE ctermbg=235 cterm=underline
highlight Statement                     ctermfg=172  cterm=NONE
highlight String                        ctermfg=107
highlight Title                         ctermfg=15
highlight Type                          ctermfg=167  cterm=NONE
highlight Visual                        ctermbg=60

highlight DiffAdd                       ctermfg=7    ctermbg=71
highlight DiffDelete                    ctermfg=7    ctermbg=52
highlight Special                       ctermfg=167

highlight pythonBuiltin                 ctermfg=73   cterm=NONE
highlight rubyBlockParameter            ctermfg=221
highlight rubyClass                     ctermfg=172
highlight rubyConstant                  ctermfg=75
highlight rubyInstanceVariable          ctermfg=146
highlight rubyInterpolation             ctermfg=107
highlight rubyLocalVariableOrMethod     ctermfg=141
highlight rubyPredefinedConstant        ctermfg=167
highlight rubyPseudoVariable            ctermfg=172
highlight rubyStringDelimiter           ctermfg=143
highlight rubyClassVariable             ctermfg=147
highlight rubyCapitalizedMethod         ctermfg=147
highlight rubyOperator                  ctermfg=172
highlight rubyBoolean                   ctermfg=172
highlight rubyRailsARAssociationMethod  ctermfg=172
highlight rubyRailsARValidationMethod   ctermfg=9
highlight rubyRailsARCallbackMethod     ctermfg=9
highlight rubyRailsARMethod             ctermfg=172
highlight rubyRailsMethod               ctermfg=172
highlight rubyRailsRenderMethod         ctermfg=9
highlight rubyRailsFilterMethod         ctermfg=9

highlight xmlTag                        ctermfg=179
highlight xmlTagName                    ctermfg=179
highlight xmlEndTag                     ctermfg=179

highlight mailSubject                   ctermfg=107
highlight mailHeaderKey                 ctermfg=221
highlight mailEmail                     ctermfg=107 cterm=underline

highlight SpellBad                      ctermfg=160 ctermbg=NONE cterm=underline
highlight SpellRare                     ctermfg=168 ctermbg=NONE cterm=underline
highlight SpellCap                      ctermfg=189 ctermbg=NONE cterm=underline
highlight MatchParent                   ctermfg=15  ctermbg=23
