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
