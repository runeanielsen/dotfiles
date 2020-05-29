call plug#begin('~/.local/share/nvim/plugged')

Plug 'sheerun/vim-polyglot'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

let g:coc_global_extensions=[ 'coc-flutter', 'coc-css', 'coc-html', 'coc-json', 'coc-yaml', 'coc-highlight', 'coc-markdownlint' ]

Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'sbdchd/neoformat'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Add quotes, html tags, etc. around something
Plug 'https://github.com/tpope/vim-surround.git'

" Consistent editing styles (indent, etc)
Plug 'https://github.com/editorconfig/editorconfig-vim.git'

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

" Init util snippets
" use <Tab> to trigger autocompletion
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

call plug#end()
