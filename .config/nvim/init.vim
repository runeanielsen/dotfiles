call plug#begin('~/.local/share/nvim/plugged')

Plug 'sheerun/vim-polyglot'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

let g:coc_global_extensions=[ 'coc-flutter', 'coc-css', 'coc-html', 'coc-json', 'coc-yaml', 'coc-highlight', 'coc-markdownlint', 'coc-prettier', 'coc-tsserver', 'coc-emmet' ]

" File search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Typescript and TSX
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'

" Automatically set pairs
Plug 'jiangmiao/auto-pairs'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Add quotes, html tags, etc. around something
Plug 'https://github.com/tpope/vim-surround.git'

" Consistent editing styles (indent, etc)
Plug 'https://github.com/editorconfig/editorconfig-vim.git'

" File Explorer
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'

let g:NERDTreeShowHidden = 1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeIgnore = []
let g:NERDTreeStatusline = ''

" Automaticaly close nvim if NERDTree is only thing left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Toggle
nnoremap <silent> <C-b> :NERDTreeToggle<CR>

" Set up proper wrapping
set wrap
set linebreak

" Relative line numbers
" au BufReadPost * set relativenumber
au BufReadPost * set number

" Disable the mouse
set mouse=c

" Instead of failing because a file isn't saved, prompt to save the file
set confirm

" Insert space characters whenever the tab key is pressed
:set expandtab

" Change all the existing tab characters to match the current tab settings,
:retab

" Init util snippets
" use <Tab> to trigger autocompletion
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" File search
nnoremap <C-p> :FZF<CR>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}

let $FZF_DEFAULT_COMMAND = 'ag -g ""'

call plug#end()

" use alt+hjkl to move between split/vsplit panels
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l
