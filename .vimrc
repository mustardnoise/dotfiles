" Maintainer:   Eugene Kruglov <mustardnoise>
" Last Change:  10 July 2017

"------------------------------------------------------------------------------
filetype off " switches file type detection, without syntax highlighting
set nocompatible " uses vim improvements
set backspace=indent,eol,start " allows to delete the character in front
                               " of the cursor
set history=200 " keeps n commands and n search patterns in the history
set ruler " displays the current cursor position in the lower right corner
set number " displays line numbers
set numberwidth=3 " number of columns used for the line number
set list listchars=tab:\ \ ,trail:· " `list` - shows <TAB> and <EOL>
                                    " `listchars` - chars for displaying
                                    "               in list mode
set textwidth=80 " maximum width of text that is being inserted
set colorcolumn=+1 " column to highlight (81)
set cursorline " highlights the screen line of the cursor
set laststatus=2 " tells when last window has status lines
set nowrap " displays long lines as just one line
set clipboard=unnamed " uses the clipboard as the unnamed register
                      " (system clipboard)
set tabstop=2 shiftwidth=2 expandtab " `tabstop` - number of spaces that <TAB>
                                     "             in file uses
                                     " `shiftwidth` - number of spaces to use
                                     "                for (auto)indent step
                                     " `expandtab` - uses spaces when <TAB>
                                     "               is inserted
set regexpengine=1 " default regexp engine to use
set ttyfast " indicates a fast terminal connection
set hidden " don't unload buffer when it is abandoned
set shell=zsh " name of shell to use for external commands
set relativenumber " shows relative line number in front of each line
set noerrorbells " disables beeping
set hlsearch " highlights matches with last search pattern
set incsearch " highlights match while typing search pattern
set ignorecase " ignores case in search patterns
set smartcase " doesn't ignore case when pattern has uppercase
set nobackup nowritebackup noswapfile " `backup` - keeps backup file after
                                      "            overwriting a file
                                      " `writebackup` - makes a backup before
                                      "                 overwriting a file
                                      " `swapfile` - whether to use a swapfile
                                      "              for a buffer
set magic " changes special characters in search patterns
" set noeol " `eol` - writes <EOL> for last line in file
" set binary " reads/writes/edits file in binary mode
set lazyredraw " don't redraw while executing macros
set showcmd " shows (partial) command in status line
set undodir=~/.vim/undo " where to store undo files
set undofile " saves undo information in a file
set undolevels=1000 " maximum number of changes that can be undone
set undoreload=10000 " maximum number of lines to save for undo on a
                     " buffer reload
set title " sets the title of the window
set scrolloff=1 " Min number of screen lines to keep above and below the cursor
filetype plugin indent on " switches on file type detection, with automatic
                          " indenting and settings
syntax on " starts using syntax highlighting
colorscheme mustardnoise-colorscheme " loads a specific color scheme
"------------------------------------------------------------------------------

"--------------------------------- VIM-PLUG --------------------------------{{{
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/bundle')
  Plug 'honza/vim-snippets' " snippets files for various programming languages
  Plug 'ecomba/vim-ruby-refactoring' " refactoring tool for ruby
  Plug 'vim-ruby/vim-ruby' " configuration files for editing and compiling ruby
  Plug 'ngmy/vim-rubocop' " runs rubocop and displays the results in vim
  Plug 'jelera/vim-javascript-syntax' " enhanced javascript syntax file
  Plug 'airblade/vim-gitgutter' " shows a git diff in the 'gutter' (sign column)
  Plug 'tpope/vim-surround' " parentheses, brackets, quotes etc
  Plug 'tpope/vim-rails' " ruby on rails power tools
  Plug 'tpope/vim-jdaddy' " json manipulation and pretty printing
  Plug 'tpope/vim-heroku' " heroku toolbelt wrapper
  Plug 'tpope/vim-haml' " runtime files for haml, sass, and scss
  Plug 'tpope/vim-fugitive' " git wrapper
  Plug 'tpope/vim-endwise' " helps to end certain structures automatically
  Plug 'tpope/vim-bundler' " support for ruby's bundler
  Plug 'tpope/vim-dotenv' " support for .env
  Plug 'tpope/vim-rhubarb' " GitHub extension for fugitive.vim
  Plug 'tpope/vim-projectionist' " project configuration (vim-test needs it)
  Plug 'ntpeters/vim-better-whitespace' " whitespace highlighting
  Plug 'vim-airline/vim-airline' " status/tabline
  Plug 'SirVer/ultisnips' " ultimate snippet solution
  Plug 'edkolev/tmuxline.vim' " tmux statusline generator
  Plug 'tomtom/tcomment_vim' " provides file-type sensible comments
  Plug 'scrooloose/syntastic' " syntax checking
  Plug 'ervandew/supertab' " performs insert mode completions with TAB
  Plug 'AndrewRadev/splitjoin.vim' " transition between multi and single-line
  Plug 'keith/rspec.vim' " rspec syntax highlighting
  Plug 'scrooloose/nerdtree' " tree explorer
  Plug 'Xuyuanp/nerdtree-git-plugin' " git status flags for NERDTree
  Plug 'othree/javascript-libraries-syntax.vim' " js libs syntax highlighting
  Plug 'Yggdroot/indentLine' " displays the indention levels
  Plug 'Konfekt/FastFold' " speeds up folds
  Plug 'Raimondi/delimitMate' " auto-completion for quotes, parens, brackets
  Plug 'ryanoasis/vim-devicons' " adds filetype icons
  Plug 'Valloric/YouCompleteMe', { 'do': './install.py' } " completion engine
  Plug 'elixir-lang/vim-elixir' " configuration files for elixir
  Plug 'moll/vim-bbye' " delete buffers (close files) without closing windows
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim' " fuzzy finder
  Plug 'sjl/gundo.vim' " visualizes undo tree
  Plug 'janko-m/vim-test' " wrapper for running tests
  Plug 'terryma/vim-multiple-cursors' " multiple selections
  Plug 'benmills/vimux' " interacts with tmux
  Plug 'kylef/apiblueprint.vim' " syntax highlighting/linting for api blueprint
  Plug 'slashmili/alchemist.vim' " elixir integration into vim
call plug#end()
"---------------------------------------------------------------------------}}}


"---------------------------------- AIRLINE --------------------------------{{{
" https://github.com/vim-airline/vim-airline
let g:airline_powerline_fonts = 1 " enables/disables automatic population of
                                  " the `g:airline_symbols` dictionary with
                                  " powerline symbols
let g:airline#extensions#tabline#enabled = 1 " enables/disables enhanced tabline
let g:airline#extensions#tabline#fnamemod = ':t' " configures the formatting
                                                 " of filenames
"---------------------------------------------------------------------------}}}


"---------------------------------- NERDTree -------------------------------{{{
" https://github.com/scrooloose/nerdtree
" highlights filetypes
function! NERDTreeHighlightFile(extension, fg, bg)
  exec 'autocmd FileType nerdtree highlight '
        \ . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg
  exec 'autocmd FileType nerdtree syn match '
        \ . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

execute NERDTreeHighlightFile('rb',     '161',     'none')
execute NERDTreeHighlightFile('rake',   '161',     'none')
execute NERDTreeHighlightFile('md',     '12',      'none')
execute NERDTreeHighlightFile('yml',    '11',      'none')
execute NERDTreeHighlightFile('json',   '6',       'none')
execute NERDTreeHighlightFile('html',   '216',     'none')
execute NERDTreeHighlightFile('erb',    '41',      'none')
execute NERDTreeHighlightFile('haml',   '169',     'none')
execute NERDTreeHighlightFile('css',    '2',       'none')
execute NERDTreeHighlightFile('scss',   '99',      'none')
execute NERDTreeHighlightFile('js',     '208',     'none')
execute NERDTreeHighlightFile('ex',     '147',     'none')
execute NERDTreeHighlightFile('exs',    '147',     'none')
"---------------------------------------------------------------------------}}}


"---------------------------------- SYNTASTIC ------------------------------{{{
" https://github.com/scrooloose/syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1 " tells syntastic to always stick
                                             " any detected errors
let g:syntastic_auto_loc_list = 0 " error window won't be automatically opened
                                  " when errors are detected, and closed when
                                  " none are detected.
let g:syntastic_check_on_open = 0 " active mode won't run syntax checks when
                                  " buffers are first loaded
let g:syntastic_check_on_wq = 0 " skips syntax checks run whenever buffers
                                " are written to disk

let g:syntastic_javascript_checkers = ['jshint']

" disables haml syntax checker
let g:loaded_syntastic_haml_haml_checker = ['']
let g:loaded_syntastic_haml_haml_lint_checker = ['']

" disables scss syntax checker
let g:loaded_syntastic_scss_sass_checker = ['']
let g:loaded_syntastic_scss_scss_lint_checker = ['']
"---------------------------------------------------------------------------}}}


"---------------------------------- GITGUTTER ------------------------------{{{
" https://github.com/airblade/vim-gitgutter
let g:gitgutter_override_sign_column_highlight = 0 " allows to customise sign
                                                   " column's background color
let g:gitgutter_realtime = 0 " disables running in realtime
let g:gitgutter_eager = 0 " disables running eagerly
"---------------------------------------------------------------------------}}}


"-------------------------------- JAVASCRIPT -------------------------------{{{
" https://github.com/othree/javascript-libraries-syntax.vim
let g:used_javascript_libs = 'jquery,underscore,handlebars' " highlights js
                                                            " libraries
"---------------------------------------------------------------------------}}}


"-------------------------------- INDENT LINE ------------------------------{{{
" https://github.com/Yggdroot/indentLine
let g:indentLine_color_term = 239 " specifies indent line color
let g:indentLine_faster = 1 " provides better performance
"---------------------------------------------------------------------------}}}


"------------------------------- YOUCOMPLETEME -----------------------------{{{
" https://github.com/Valloric/YouCompleteMe
let g:ycm_key_list_select_completion = ['<C-TAB>', '<Down>'] " controls the key
                                                             " mappings used to
                                                             " select the first
                                                             " completion
let g:ycm_key_list_previous_completion = ['<C-S-TAB>', '<Up>'] " controls the
                                                               " key mappings
                                                               " used to select
                                                               " the previous
                                                               " completion
" controls for which vim filetypes should the semantic completion engine
" be turned off
let g:ycm_filetype_specific_completion_to_disable = {
      \ 'ruby': 1
      \}
"---------------------------------------------------------------------------}}}


"--------------------------------- SUPERTAB --------------------------------{{{
" https://github.com/ervandew/supertab
let g:SuperTabDefaultCompletionType = '<C-TAB>' " sets the default
                                                " completion type
"---------------------------------------------------------------------------}}}


"----------------------------------- RUBY ----------------------------------{{{
" https://github.com/vim-ruby/vim-ruby
let g:ruby_path = system('rvm current')
let ruby_operators = 1 " highlights ruby operators
let ruby_no_expensive = 1 " turns off the `end` keyword colorizing according
                          " to the opening statement of the block it closes
                          " (performance)
"---------------------------------------------------------------------------}}}


"----------------------------------- FASTFOLD ------------------------------{{{
" https://github.com/Konfekt/FastFold
set foldmethod=indent " folding type
set foldminlines=10 " minimum number of lines for a fold to be closed
set foldnestmax=20 " maximum fold depth
let ruby_fold = 1 " enables ruby folding
let javaScript_fold = 1 " enables js folding
let vimsyn_folding = 'af' " enables vim script folding
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker foldminlines=3
augroup END
"---------------------------------------------------------------------------}}}


"----------------------------------- BUFFER --------------------------------{{{
" returns to last edit position when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
set viminfo^=% " remembers info about open buffers on close
"---------------------------------------------------------------------------}}}


"---------------------------------- DEVICONS -------------------------------{{{
" https://github.com/ryanoasis/vim-devicons
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1 " forces extra padding in
                                                  " NERDTree so that the
                                                  " filetype icons line up
                                                  " vertically
"---------------------------------------------------------------------------}}}


"------------------------------------- FZF ---------------------------------{{{
let g:fzf_command_prefix = 'FZF' " gives the prefix to fzf commands
" adds command alias
cnoreabbrev Ag FZFAg
"---------------------------------------------------------------------------}}}


"----------------------------------- MAPPINGS ------------------------------{{{
let mapleader = "\<Space>"

" opens / closes NERDTree
nnoremap <Leader>n :NERDTreeToggle<CR>

" https://github.com/janko-m/vim-test
" runs the test nearest to the cursor
nnoremap <silent> <Leader>s :TestNearest<CR>
" runs all tests in the current file
nnoremap <silent> <Leader>t :TestFile<CR>
" runs the whole test suite
nnoremap <silent> <Leader>a :TestSuite<CR>
" runs the last test
nnoremap <silent> <Leader>l :TestLast<CR>
" visits the test file from which you last run your tests
nnoremap <silent> <Leader>g :TestVisit<CR>

" goes next buffer
nnoremap <Leader>bn :bnext<CR>
" goes to previous buffer
nnoremap <Leader>bp :bprevious<CR>

" deletes buffer
nnoremap <Leader>bd :Bdelete<CR>

" runs `rails console` with foreman
nnoremap <Leader>rc :call
  \ VimuxRunCommand("clear; foreman run rails console")<CR>

" runs `rake db:migrate`
nnoremap <Leader>rdm :Rake db:migrate<CR>

" runs `rake routes`
nnoremap <Leader>rr :call VimuxRunCommand("clear; bundle exec rake routes")<CR>

 " zooms the runner pane (use <bind-key> z to restore runner pane)
nnoremap <Leader>vz :call VimuxZoomRunner()<CR>

" runs `bundle install`
nnoremap <Leader>bi :Bundle install<CR>

" runs `rubocop`
nnoremap <Leader>ru :Rubocop<CR>

" closes file
nnoremap <Leader>q :q<CR>

" saves file
nnoremap <Leader>w :w<CR>

" saves and closes file
nnoremap <Leader>wq :wq<CR>

" opens gundo panel
nnoremap <Leader>gdt :GundoToggle<CR>

" git blame
nnoremap <Leader>gb :Gblame<CR>

" force to not use arrow keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" removes seach highlight
nnoremap <Leader>/ :nohlsearch<CR>

" adds frozen string literal comment
nnoremap <Leader>fsl :call append(0, "# frozen_string_literal: true")<CR>

" runs FZF
nnoremap <C-P> :FZF<CR>

" opens .vimrc in vertical split
nnoremap <Leader>ev :vsplit $MYVIMRC<CR>

" sources .vimrc
nnoremap <Leader>sv :source $MYVIMRC<CR>
"---------------------------------------------------------------------------}}}
