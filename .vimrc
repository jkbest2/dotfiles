" Enable Powerline
set rtp+=/usr/lib/python3.5/site-packages/powerline/bindings/vim
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup
set laststatus=2

" Tab options
set tabstop=4                   " Number of spaces per tab
set softtabstop=4               " Number of spaces in tab when editing
set expandtab                   " Tabs are spaces
set shiftwidth=4
set smarttab
" filetype plugin indent on       " Allow for hard tabs in eg Makefiles

" UI options
" set background=dark
" colorscheme solarized           " Color scheme
" set guifont=Fira\ Mono\ for\ Powerline\ 11 " Set font and size
set number                      " Show line numbers
" set cursorline                  " Highlight current line
set colorcolumn=80              " Show 80-char line length
set wildmenu                    " Visual autocomplete for command menu

" Nice for programming
syntax on                       " Turn syntax highlighting on
filetype indent on              " Load file-type specific indentation rules
set showmatch                   " Replace matching parens/brackets

" Search options
set incsearch                   " Search as characters are entered
set hlsearch                    " Highlight search results

" Key rebindings
" End search highlighting with ",<spc>"
nnoremap <leader><space> :nohlsearch<CR>
nnoremap gV `[v`]               " highlight last inserted text
" nnoremap j gj                   " Move down one *visual* line
" nnoremap k gk                   " Move up one *visual* line

" Vim-Slime/tmux setup
let g:slime_target = "tmux"

" Vim-latex target: pdf
let g:Tex_DefaultTargetFormat='pdf'

" powerline symbols
" let g:airline_powerline_fonts = 1

" Saner delete defaults
set backspace=indent,eol,start


