filetype off
set nocompatible
set history=200
set ruler
set number
set numberwidth=3
set background=dark
set list listchars=tab:\ \ ,trail:·
set textwidth=80
set colorcolumn=+1
set cursorline
" set mouse=a
set laststatus=2
set nowrap
set clipboard=unnamed
set ts=2 sw=2 et
set regexpengine=1
set ttyfast
set hid
set shell=zsh
" set norelativenumber
set relativenumber
set noerrorbells
set hlsearch
set incsearch
set nobackup nowritebackup noswapfile
set magic
" set noeol
" set binary
set lazyredraw
set showcmd
filetype plugin indent on
syntax on
colorscheme mustardnoise-railscasts

"---------------------------------------- AG ----------------------------------
" https://github.com/rking/ag.vim
let g:ackprg = 'ag --vimgrep'
"------------------------------------------------------------------------------


"------------------------------------ CTRL-P ----------------------------------
" https://github.com/ctrlpvim/ctrlp.vim
set runtimepath^=~/.vim/bundle/ctrlp.vim

" https://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
let g:ctrlp_use_caching = 0
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor

    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
else
  let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
  let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<space>', '<cr>', '<2-LeftMouse>'],
    \ }
endif
"------------------------------------------------------------------------------


"---------------------------------- AIRLINE -----------------------------------
" https://github.com/vim-airline/vim-airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
"------------------------------------------------------------------------------


"---------------------------------- DEVICONS ----------------------------------
" https://github.com/ryanoasis/vim-devicons
set guifont=Droid\ Sans\ Mono\ for\ Powerline\ Plus\ Nerd\ File\ Types:h11
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
"------------------------------------------------------------------------------


"---------------------------------- NERDTree ----------------------------------
" https://github.com/scrooloose/nerdtree
let NERDTreeShowHidden = 0
let NERDTreeWinSize = 31

function! NERDTreeHighlightFile(extension, fg, bg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

execute NERDTreeHighlightFile('rb',     '161',     'none')
execute NERDTreeHighlightFile('md',     'blue',    'none')
execute NERDTreeHighlightFile('yml',    'yellow',  'none')
execute NERDTreeHighlightFile('json',   '6',       'none')
execute NERDTreeHighlightFile('html',   '216',     'none')
execute NERDTreeHighlightFile('erb',    '41',      'none')
execute NERDTreeHighlightFile('haml',   '169',     'none')
execute NERDTreeHighlightFile('css',    '2',       'none')
execute NERDTreeHighlightFile('scss',   '99',      'none')
execute NERDTreeHighlightFile('js',     '208',     'none')
"------------------------------------------------------------------------------


"---------------------------------- SYNTASTIC ---------------------------------
" https://github.com/scrooloose/syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_javascript_checkers = ['jshint']

" disable HAML syntax checker
let g:loaded_syntastic_haml_haml_checker = ['']
let g:loaded_syntastic_haml_haml_lint_checker = ['']

" disable SCSS syntax checker
let g:loaded_syntastic_scss_sass_checker = ['']
let g:loaded_syntastic_scss_scss_lint_checker = ['']
"------------------------------------------------------------------------------


"---------------------------------- GITGUTTER ---------------------------------
" https://github.com/airblade/vim-gitgutter
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_realtime = 0
let g:gitgutter_eager = 0
"------------------------------------------------------------------------------


"-------------------------------- JAVASCRIPT ----------------------------------
" https://github.com/pangloss/vim-javascript
" let g:javascript_enable_domhtmlcss = 1

" https://github.com/othree/javascript-libraries-syntax.vim
let g:used_javascript_libs = 'jquery,underscore,handlebars'
"------------------------------------------------------------------------------


"-------------------------------- INDENT LINE ---------------------------------
" https://github.com/Yggdroot/indentLine
let g:indentLine_color_term = 239
let g:indentLine_faster = 1
"------------------------------------------------------------------------------


"---------------------------------- RSPEC -------------------------------------
" https://github.com/thoughtbot/vim-rspec
let g:rspec_runner = "os_x_iterm2"
noremap <C-@> <C-x><C-o>
"------------------------------------------------------------------------------


"--------------------------------- VIM-PLUG -----------------------------------
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/bundle')
  Plug 'honza/vim-snippets'
  Plug 'ecomba/vim-ruby-refactoring'
  Plug 'vim-ruby/vim-ruby'
  Plug 'ngmy/vim-rubocop'
  Plug 'thoughtbot/vim-rspec'
  Plug 'jelera/vim-javascript-syntax'
  Plug 'airblade/vim-gitgutter'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-rvm'
  Plug 'tpope/vim-rails'
  Plug 'tpope/vim-jdaddy'
  Plug 'tpope/vim-heroku'
  Plug 'tpope/vim-haml'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-endwise'
  Plug 'tpope/vim-dispatch'
  Plug 'tpope/vim-bundler'
  Plug 'tpope/vim-dotenv'
  Plug 'ryanoasis/vim-devicons'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'vim-airline/vim-airline'
  Plug 'SirVer/ultisnips'
  Plug 'edkolev/tmuxline.vim'
  Plug 'tomtom/tcomment_vim'
  Plug 'majutsushi/tagbar'
  Plug 'scrooloose/syntastic'
  Plug 'ervandew/supertab'
  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'keith/rspec.vim'
  Plug 'scrooloose/nerdtree'
  Plug 'Xuyuanp/nerdtree-git-plugin'
  Plug 'othree/javascript-libraries-syntax.vim'
  Plug 'Yggdroot/indentLine'
  Plug 'Konfekt/FastFold'
  Plug 'Raimondi/delimitMate'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'wincent/command-t'
  Plug 'chriskempson/base16-vim'
  Plug 'rking/ag.vim'
  Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
call plug#end()
"------------------------------------------------------------------------------


"------------------------------- YOUCOMPLETEME --------------------------------
" https://github.com/Valloric/YouCompleteMe
let g:ycm_key_list_select_completion = ['<C-TAB>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-S-TAB>', '<Up>']
let g:ycm_collect_identifiers_from_tags_files = 0
"------------------------------------------------------------------------------


"--------------------------------- SUPERTAB -----------------------------------
" https://github.com/ervandew/supertab
let g:SuperTabDefaultCompletionType = '<C-Tab>'
"------------------------------------------------------------------------------


"----------------------------------- RUBY -------------------------------------
" https://github.com/vim-ruby/vim-ruby
let g:ruby_path = system('rvm current')
let ruby_operators = 1
let ruby_no_expensive = 1
"------------------------------------------------------------------------------


"----------------------------------- FASTFOLD ---------------------------------
" https://github.com/Konfekt/FastFold
set foldmethod=indent
set foldminlines=10
set foldnestmax=20
let ruby_fold = 1
let javaScript_fold = 1
let vimsyn_folding = 'af'
"------------------------------------------------------------------------------


"----------------------------------- BUFFER -----------------------------------
" Return to last edit position when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%
"------------------------------------------------------------------------------


"----------------------------------- MAPPINGS ---------------------------------
let mapleader = "\<Space>"

nmap <Leader>n :NERDTreeToggle<CR>
nmap <Leader>t :call RunCurrentSpecFile()<CR>
nmap <Leader>s :call RunNearestSpec()<CR>
nmap <Leader>l :call RunLastSpec()<CR>
nmap <Leader>a :call RunAllSpecs()<CR>

" Next and previous buffer
map <Leader>bn :bnext<CR>
map <Leader>bp :bprevious<CR>

" Run rails console with Foreman
map <Leader>rc :Dispatch foreman run rails c<CR>

" Run rake db:migrate
map <Leader>rdm :Rake db:migrate<CR>

" Run bundle install
map <Leader>bi :Bundle install<CR>

" Run Rubocop
map <Leader>ru :Rubocop<CR>

" Close file
map <Leader>q :q<CR>

" Save file
map <Leader>w :w<CR>

" Save and close file
map <Leader>wq :wq<CR>

" force to not use arrow keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
"------------------------------------------------------------------------------
