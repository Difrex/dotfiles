" Colorscheme
set t_Co=256
colorscheme xoria256
" set background=dark

" Syntax
syntax on

" Numbers
set number

" Maps
inoremap { {  }<LEFT><LEFT>
inoremap ' '  '<LEFT><LEFT>
inoremap " "  "<LEFT><LEFT>
inoremap [ [  ]<LEFT><LEFT>
inoremap ( (  )<LEFT><LEFT>

" Ignore VI
set nocompatible

" Commands
set showcmd

set showmatch

" Tabs
set smartindent
set autoindent
set tabstop=4
set shiftwidth=4
set pastetoggle=

" Search
set hlsearch
set nohlsearch
set smartcase
set nowrapscan

" History
set history=128
set undolevels=1000
set infercase

" Swap
set swapfile
set dir=~/.vim/swap/

" Backups
set backup
set backupdir=~/.vim/backup/

" Buffers
nmap <F4> <Esc>:buffers<CR>
vmap <F4> <Esc>:buffers<CR>
imap <F4> <Esc><Esc>:buffers<CR>
" Previous
map <F5> :bp<CR>
vmap <F5> <Esc>:bp<CR>i
imap <F5> <Esc>:bp<CR>i
" Next
map <F6> :bn<CR>
vmap <F6> <Esc>:bn<CR>i
imap <F6> <Esc>:bn<CR>i

" Autoread
set autoread

" Cursor
set cursorline

" set showtabline=2
set title
set titlestring=%t%(\ %m%)%(\ %r%)%(\ %h%)%(\ %w%)%(\ (%{expand(\"%:p:~:h\")})%)\ -\ VIM
set confirm
set statusline=%<%f%h%m%r%=format=%{&fileformat}\ file=%{&fileencoding}\ enc=%{&encoding}\ %b\ 0x%B\ %l,%c%V\ %P
