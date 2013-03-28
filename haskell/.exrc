if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
map! <F1> <F1>
noremap Y y$
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
omap <F1> <F1>
vmap <F1> <F1>
nnoremap <F1> :help 
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set backspace=indent,eol,start
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set helplang=en
set hidden
set history=50
set hlsearch
set ignorecase
set incsearch
set nojoinspaces
set lazyredraw
set modelines=0
set path=.,/usr/include,,,**
set ruler
set scrolloff=3
set shiftwidth=2
set showcmd
set showmatch
set smartcase
set smartindent
set smarttab
set tabstop=2
set tags=./tags,tags,java.tags
set title
set wildmenu
set wildmode=list:longest,full
set window=0
" vim: set ft=vim :
