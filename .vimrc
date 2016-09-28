filetype off
set nocompatible
set backspace=indent,eol,start
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

let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
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


"--------------------------------- VIM-PLUG -----------------------------------
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/bundle')
  Plug 'honza/vim-snippets'
  Plug 'ecomba/vim-ruby-refactoring'
  Plug 'vim-ruby/vim-ruby'
  Plug 'ngmy/vim-rubocop'
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
  Plug 'chriskempson/base16-vim'
  Plug 'rking/ag.vim'
  Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
  Plug 'elixir-lang/vim-elixir'
  Plug 'moll/vim-bbye'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'sjl/gundo.vim'
  Plug 'janko-m/vim-test'
  Plug 'terryma/vim-multiple-cursors'
  Plug 'benmills/vimux'
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

nmap <silent> <Leader>s :TestNearest<CR>
nmap <silent> <Leader>t :TestFile<CR>
nmap <silent> <Leader>a :TestSuite<CR>
nmap <silent> <Leader>l :TestLast<CR>
nmap <silent> <Leader>g :TestVisit<CR>

" Next and previous buffer
map <Leader>bn :bnext<CR>
map <Leader>bp :bprevious<CR>

" Delete Buffer
map <Leader>bd :Bdelete<CR>

" Run rails console with Foreman
map <Leader>rc :call VimuxRunCommand("clear; foreman run rails console")<CR>

" Run rake db:migrate
map <Leader>rdm :Rake db:migrate<CR>

" Run rake routes
map <Leader>rr :call VimuxRunCommand("clear; bundle exec rake routes")<CR>

" Zoom the runner pane (use <bind-key> z to restore runner pane)
map <Leader>vz :call VimuxZoomRunner()<CR>

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

" Open Gundo panel
nnoremap <leader>gdt :GundoToggle<CR>

" Git blame
nmap <Leader>gb :Gblame<CR>

" force to not use arrow keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
"------------------------------------------------------------------------------
