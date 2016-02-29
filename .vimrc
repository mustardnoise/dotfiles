execute pathogen#infect()
set nocompatible
filetype off
syntax enable
set history=700
filetype plugin on
filetype indent on
set ruler
set number
set numberwidth=3
set background=dark
set list listchars=tab:\ \ ,trail:·
set textwidth=80
set colorcolumn=+1
set cursorline
if has('mouse')
  set mouse=a
endif
set laststatus=2
set nowrap
set clipboard=unnamed
set ts=2 sw=2 et
set regexpengine=1
set ttyfast
set shell=zsh
set norelativenumber
set noerrorbells
set hlsearch
set nobackup nowritebackup noswapfile
set magic
" set noeol
" set binary
" set lazyredraw

"================================== SILVER SEARCHER ==========================
" https://github.com/rking/ag.vim
let g:ackprg = 'ag --vimgrep'
"=============================================================================


"================================== CTRLP ====================================
" https://github.com/kien/ctrlp.vim
"set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
"=============================================================================


"================================== AIRLINE ==================================
" https://github.com/vim-airline/vim-airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
"=============================================================================


"================================== DEVICONS =================================
" https://github.com/ryanoasis/vim-devicons
set guifont=Droid\ Sans\ Mono\ for\ Powerline\ Plus\ Nerd\ File\ Types:h11
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
"=============================================================================


"================================== NERDTree =================================
" https://github.com/scrooloose/nerdtree
let NERDTreeShowHidden = 0
let NERDTreeWinSize = 31

function! NERDTreeHighlightFile(extension, fg, bg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('rb',     '161',    'none')
call NERDTreeHighlightFile('md',     'blue',   'none')
call NERDTreeHighlightFile('yml',    'yellow', 'none')
call NERDTreeHighlightFile('json',   '6',      'none')
call NERDTreeHighlightFile('html',   '216',    'none')
call NERDTreeHighlightFile('erb',    '41',     'none')
call NERDTreeHighlightFile('haml',   '169',    'none')
call NERDTreeHighlightFile('css',    '2',      'none')
call NERDTreeHighlightFile('scss',   '99',     'none')
call NERDTreeHighlightFile('js',     '208',    'none')
"=============================================================================


"================================== SYNTASTIC ================================
" https://github.com/scrooloose/syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_javascript_checkers = ['jshint']

" disable HAML syntax checker
let g:loaded_syntastic_haml_haml_checker = ['']
let g:loaded_syntastic_haml_haml_lint_checker = ['']

" disable SCSS syntax checker
let g:loaded_syntastic_scss_sass_checker = ['']
let g:loaded_syntastic_scss_scss_lint_checker = ['']
"=============================================================================


"================================== GITGUTTER ================================
" https://github.com/airblade/vim-gitgutter
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_realtime = 0
let g:gitgutter_eager = 0
"=============================================================================


"================================ JAVASCRIPT =================================
" https://github.com/pangloss/vim-javascript
let g:javascript_enable_domhtmlcss = 1

" https://github.com/othree/javascript-libraries-syntax.vim
let g:used_javascript_libs = 'jquery,underscore,handlebars'
"=============================================================================


"================================ INDENT LINE ================================
" https://github.com/Yggdroot/indentLine
let g:indentLine_color_term = 239
let g:indentLine_faster = 1
"=============================================================================


"================================== RSPEC ====================================
" https://github.com/thoughtbot/vim-rspec
let g:rspec_runner = "os_x_iterm2"
noremap <C-@> <C-x><C-o>
"=============================================================================


"================================== VUNDLE ===================================
" https://github.com/VundleVim/Vundle.vim
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
  Plugin 'gmarik/Vundle.vim'
  Plugin 'Valloric/YouCompleteMe'
  Plugin 'thoughtbot/vim-rspec'
  Plugin 'Xuyuanp/nerdtree-git-plugin'
  Plugin 'SirVer/ultisnips'
  Plugin 'honza/vim-snippets'
call vundle#end()
"=============================================================================


"=============================== YOUCOMPLETEME ===============================
" https://github.com/Valloric/YouCompleteMe
let g:ycm_key_list_select_completion = ['<C-TAB>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-S-TAB>', '<Up>']
let g:ycm_collect_identifiers_from_tags_files = 0
"=============================================================================


"================================= SUPERTAB ==================================
" https://github.com/ervandew/supertab
let g:SuperTabDefaultCompletionType = '<C-Tab>'
"=============================================================================


"=================================== RUBY ====================================
" https://github.com/vim-ruby/vim-ruby
let g:ruby_path = system('rvm current')
let ruby_operators = 1
let ruby_no_expensive = 1
"=============================================================================


"=================================== FASTFOLD ================================
" https://github.com/Konfekt/FastFold
set foldmethod=indent
set foldminlines=6
set foldnestmax=10
let ruby_fold = 1
let javaScript_fold = 1
let vimsyn_folding = 'af'
"=============================================================================


"=================================== BUFFER ==================================
" Return to last edit position when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%
"=============================================================================


"=================================== MAPPINGS ================================
nmap <Leader>n :NERDTreeToggle<CR>
nmap <Leader>t :call RunCurrentSpecFile()<CR>
nmap <Leader>s :call RunNearestSpec()<CR>
nmap <Leader>l :call RunLastSpec()<CR>
nmap <Leader>a :call RunAllSpecs()<CR>
"=============================================================================

colorscheme mustardnoise-railscasts
