" My Vim Configuration File
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                  " VIM SETTINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TURNS LINE NUMBERING ON
set nu

" ENABLES A SYSTEM-WIDE VIMRC
set nocompatible 

" ENSURES DEFVAULT VIM SYNTAX HIGHLIGHTING
syntax on

" ENABLE MOUSE USE IN ALL MODES
set mouse=a

" ENABLE SAVING OF TAB TITLES FOR SESSIONS 
set sessionoptions+=tabpages,globals

set encoding=utf-8

" SET TAB WIDTHS
set expandtab
set tabstop=4
set shiftwidth=4

" FILE-TYPE SPECIFIC INDENTATION
autocmd Filetype r setlocal tabstop=2 shiftwidth=2
autocmd Filetype cpp setlocal tabstop=2 shiftwidth=4
autocmd Filetype julia setlocal tabstop=4 shiftwidth=4

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                    " PLUGINS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ENABLES PLUGINS TO WORK
filetype plugin on

" LOCATION OF WHERE PLUGINS ARE INSTALLED
call plug#begin('~/.local/share/nvim/addons')

" Git wrapper for Vim
Plug 'tpope/vim-fugitive'

" Preview markdown on your modern browser with synchronised scrolling and flexible configuration.
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }

" Automatic Window Resizing
Plug 'camspiers/lens.vim'

" Support for Julia
Plug 'JuliaEditorSupport/julia-vim'

" Formatting Julia Files
Plug 'kdheepak/JuliaFormatter.vim'

" Formatting of HTML, JS, CSS, JSON, and JSX Files
" Plug 'maksimr/vim-jsbeautify'

" Adding support for LaTeX
Plug 'lervag/vimtex'

" Live Previewing of LaTeX Documents
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

" Moving through Vim easily
Plug 'easymotion/vim-easymotion'

" Emoji support
Plug 'fszymanski/deoplete-emoji'

" Command line fuzzy finder
Plug 'junegunn/fzf', { 'do': './install --bin' } "Checks latest fzf binary
Plug 'junegunn/fzf.vim'

" Easy commenting for Vim
Plug 'preservim/nerdcommenter'

" Gruvbox color theme for Vim 
Plug 'morhetz/gruvbox'

" Sends text to a target for execution
Plug 'jpalardy/vim-slime'

" Surrounding words with characters in Vim
Plug 'tpope/vim-surround'

" BibTeX Handling
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax' "Also used for Markdown formatting

" VIM Table Mode for instant table creation.
Plug 'dhruvasagar/vim-table-mode'

" Deoplete Completion framework
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

" Tab naming powers
Plug 'gcmt/taboo.vim'

" Multiple cursors for editing
Plug 'mg979/vim-visual-multi'

" Adds file type icons to Vim plugins
Plug 'ryanoasis/vim-devicons'

" The NERDTree is a file system explorer for the Vim editor
Plug 'preservim/nerdtree'

" Distraction-free writing in Vim
Plug 'junegunn/goyo.vim'

" Vim session saving
Plug 'tpope/vim-obsession'

" Rethinking Vim as a tool for writing
Plug 'reedes/vim-pencil'

" Lean & mean status/tabline for vim that's light as air
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes' "Installs themes for airline

" Editorconfig support
Plug 'editorconfig/editorconfig-vim'

" vim-tmux navigator
Plug 'christoomey/vim-tmux-navigator'

" Neoranger
Plug 'Lokaltog/neoranger'

" VOoM Outliner
Plug 'vim-voom/VOoM'

" Nvim-R
Plug 'jalvesaq/Nvim-R'

" R devtools plugin
Plug 'mllg/vim-devtools-plugin', { 'for': ['r', 'rmd', 'rnoweb']} 

call plug#end()
 
 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                 " PLUGIN SETTINGS
 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 
 """""""""""""""""""""
 "JULIA FORMATTER
 """""""""""""""""""""
 
 let g:JuliaFormatter_options = {
         \ 'indent'                    : 4,
         \ 'margin'                    : 92,
         \ 'always_for_in'             : v:false,
        \ 'whitespace_typedefs'       : v:false,
        \ 'whitespace_ops_in_indices' : v:true,
        \ }

"""""""""""""""""
"MARKDOWN-PREVIEW
"""""""""""""""""

" ${name} will be replace with the file name
let g:mkdp_page_title = '「${name}」'
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1,
    \ 'sequence_diagrams': {},
    \ 'flowchart_diagrams': {}
    \ }

let g:mkdp_browser = 'firefox'

"""""""""""""""""""""
" AIRLINE
"""""""""""""""""""""

" Enabling Powerline symbols
let g:airline_powerline_fonts = 1

" Allows word counting in the following filetypes
let g:airline#extensions#wordcount#filetypes = '\vasciidoc|help|mail|markdown|pandoc|org|rst|tex|text'

" Shows all buffers when only one tab open
let g:airline#extensions#tabline#enabled = 0

" Handles file path displays
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

" Sets theme for airline
let g:airline_theme='gruvbox'

""""""""""""""""""""""""""""""
" VIM-JSBEAUTIFY 
""""""""""""""""""""""""""""""
".vimrc
" map <c-f> :call JsBeautify()<cr>
" or
" autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
" for json
" autocmd FileType json noremap <buffer> <c-f> :call JsonBeautify()<cr>
" for jsx
" autocmd FileType jsx noremap <buffer> <c-f> :call JsxBeautify()<cr>
" for html
" autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
" for css or scss
" autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>

""""""""""""""""""""""""""""""
" VIM-EMOJI
""""""""""""""""""""""""""""""
set completefunc=emoji#complete

""""""""""""""""""""""""""""""
" VIMPENCIL
""""""""""""""""""""""""""""""

" Automatically enable Pencil for files
augroup pencil
  autocmd!
  autocmd FileType py call pencil#init({'wrap' : 'soft'})
  autocmd FileType markdown call pencil#init({'wrap' : 'soft'})
  autocmd FileType julia call pencil#init({'wrap' : 'soft'})
  autocmd FileType tex call pencil#init({'wrap' : 'soft'})
augroup END

""""""""""""""""""""""""""""""
" GRUVBOX
""""""""""""""""""""""""""""""
let g:gruvbox_termcolors=256
let g:gruvbox_contrast_dark = 'medium'
colorscheme gruvbox
set background=dark " Setting dark mode

""""""""""""""""""""""""""""""
" TABULAR & VIM-MARKDOWN
""""""""""""""""""""""""""""""

let g:vim_markdown_folding_level = 1

""""""""""""""""""""""""""""""
" VIM-PANDOC
""""""""""""""""""""""""""""""
let g:pandoc#filetypes#handled = ['pandoc', 'markdown'] 
let g:pandoc#modules#disabled = ['spell']
let g:pandoc#folding#fold_fenced_codeblocks = 1
let g:pandoc#folding#fold_yaml = 1
let g:pandoc#biblio#bibs = ['/home/src/Knowledgebase/Zettelkasten/zettel.bib']
let g:pandoc#toc#close_after_navigating = 0
let g:pandoc#toc#position = 'bottom' 
let g:pandoc#folding#fdc = 0

""""""""""""""""""""""""""""""
" VIM-PANDOC-SYNTAX
""""""""""""""""""""""""""""""
let g:pandoc#syntax#conceal#blacklist = ['strikeout', 'list', 'quotes']

""""""""""""""""""""""""""""""
" DEOPLETE
""""""""""""""""""""""""""""""

" Turns on Deoplete at start-up of Vim
let g:deoplete#enable_at_startup = 1

" Chooses backend for bibtex autocompletion
 "let g:pandoc#completion#bib#mode = 'citeproc'

" Disables autocompletion while writing
"call deoplete#custom#option('auto_complete', v:false)

" Enables omnicompletion of citation keys
call deoplete#custom#var('omni', 'input_patterns', {
    			\ 'pandoc': '@'
    			\})

" Enables deoplete for tex files
call deoplete#custom#var('omni', 'input_patterns', {
          \ 'tex': g:vimtex#re#deoplete
          \})

""""""""""""""""""""""""""""""
" VIM-SLIME 
""""""""""""""""""""""""""""""

let g:slime_target = "tmux"
let g:slime_paste_file = "$HOME/.slime_paste"

""""""""""""""""""""""""""""""
" FZF-VIM
""""""""""""""""""""""""""""""

let $FZF_DEFAULT_OPTS='--reverse' 
let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8 } }
let g:fzf_history_dir = '~/.local/share/fzf-history'
let $FZF_DEFAULT_COMMAND = 'rg --files --hidden'
"
""""""""""""""""""""""""""""""
" NERDCommenter
""""""""""""""""""""""""""""""

" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code indentation
" let g:NERDDefaultAlign = 'left'
" Set a language to use its alternate delimiters by default
"let g:NERDAltDelims_java = 1
" Add your own custom formats or override the defaults
"let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
" Enable NERDCommenterToggle to check all selected lines is commented or not 
let g:NERDToggleCheckAllLines = 1

""""""""""""""""""""""""""""""
" VIM-TEX 
""""""""""""""""""""""""""""""

let g:tex_flavor='latexmk'
let g:vimtex_view_general_viewer = 'okular'
let g:vimtex_view_general_options = '--unique file:@pdf\#src:@line@tex'
let g:vimtex_view_general_options_latexmk = '--unique'
let g:vimtex_quickfix_mode=0
let g:vimtex_fold_enabled = 1
let g:vimtex_fold_types = {
           \ 'preamble' : {'enabled' : 1},
           \ 'sections' : {'enabled' : 0},
           \ 'envs' : {
           \   'blacklist' : ['figures'],
           \ },
           \}


""""""""""""""""""""""""""""""
" VIM-LATEX-LIVE-PREVIEW 
""""""""""""""""""""""""""""""
let g:livepreview_previewer = 'okular'
let g:livepreview_use_biber = 1

""""""""""""""""""""""""""""""
" VIM-TABLE-MODE
""""""""""""""""""""""""""""""

let g:table_mode_corner = "|"
let g:table_mode_align_char = ":"

function! s:isAtStartOfLine(mapping)
  let text_before_cursor = getline('.')[0 : col('.')-1]
  let mapping_pattern = '\V' . escape(a:mapping, '\')
  let comment_pattern = '\V' . escape(substitute(&l:commentstring, '%s.*$', '', ''), '\')
  return (text_before_cursor =~? '^' . ('\v(' . comment_pattern . '\v)?') . '\s*\v' . mapping_pattern . '\v$')
endfunction

inoreabbrev <expr> <bar><bar>
          \ <SID>isAtStartOfLine('\|\|') ?
          \ '<c-o>:TableModeEnable<cr><bar><space><bar><left><left>' : '<bar><bar>'
inoreabbrev <expr> __
          \ <SID>isAtStartOfLine('__') ?
          \ '<c-o>:silent! TableModeDisable<cr>' : '__'


""""""""""""""""""""""""""""""
" EDITORCONFIG
""""""""""""""""""""""""""""""
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

""""""""""""""""""""""""""""""
" NEORANGER
""""""""""""""""""""""""""""""
let g:neoranger_viewmode='miller'

""""""""""""""""""""""""""""""
" Nvim-R
""""""""""""""""""""""""""""""
" Open in external terminal (better hilighting and use i3 window management
let R_external_term = 'kitty --title=R -e'
" Fancy REPL
" let R_app = 'radian'
" Don't insert <- for _
let R_assign = 0
" Inster Rmd chunk with two backticks
let R_rmdchunk = '``'
let R_args = ['--no-save', '--no-restore-history']
" Knit Rmd in a fresh environment (as in RStudio
let R_rmd_environment = 'new.env()'
" Clear REPL input before sending
let R_clear_line = 1
" Open PDF after knitting
let R_openpdf = 1
" Open HTML or reload after knitting
let R_openhtml = 1
" Use visidata to open CSV files
let R_csv_app = 'terminal:vd'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                  " VIM FUNCTIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" code/string searching tool for multifile exploration
let g:ackprg = 'rg --nogroup --nocolor --column'

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit',
  \ 'ctrl-o': ':r !echo',
  \ }

augroup vimrc_todo
    au!
    au Syntax * syn match MyTodo /\v<(FIXME|NOTE|TODO|OPTIMIZE|DESC):/
          \ containedin=.*Comment,vimCommentTitle
augroup END

function! TwiddleCase(str)
  if a:str ==# toupper(a:str)
    let result = tolower(a:str)
  elseif a:str ==# tolower(a:str)
    let result = substitute(a:str,'\(\<\w\+\>\)', '\u\1', 'g')
  else
    let result = toupper(a:str)
  endif
  return result
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                 " KEY REMAPS 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Enables Tab completion for selecting from Deoplete omnicompletion
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" Enables ripgrep for file completion via fzf
inoremap <expr> <c-x><c-f> fzf#vim#complete#path('rg --files')

" Maps leader to the spacebar
" map <Space> <Leader>
" map , <LocalLeader>
let mapleader = "\<Space>"
let maplocalleader = ","

" Maps easymotion jumps for lines
map <leader><space>l <Plug>(easymotion-bd-jk)
nmap <leader><space>l <Plug>(easymotion-overwin-line)

" Maps easymotion jumps for words
map  <leader><Space>w <Plug>(easymotion-bd-w)
nmap <leader><Space>w <Plug>(easymotion-overwin-w)

" Automatic formatting for Julia files
nnoremap <buffer>  <c-f> :JuliaFormatterFormat<cr> 

" Maps quit
noremap <leader>q :q<cr>

" Maps quit all  
noremap <c-q> :qa<cr>

" Maps write
 nnoremap <leader>w :w<cr>

" Maps ripgrep file searching function
nnoremap <C-g> :Rg<Cr>

" Maps display of current buffers 
nnoremap <C-b> :Buffers<Cr>

" Deselects currently highlighted searches 
nnoremap <Leader><BS> :noh<cr>

" Activates Twiddle case to switch between cases of selected text
vnoremap ~ y:call setreg('', TwiddleCase(@"), getregtype(''))<CR>gv""Pgv

" File navigation
nmap <leader>fr :History<CR>
nmap <leader>fF :RangerCurrentFile<CR>
nmap <leader>ff :Files<CR>
nmap <leader>fd :call fzf#run(fzf#wrap({'source': 'fasd -d -R', 'sink': { line -> execute('cd '.split(line)[-1]) }}))<CR>
nmap <leader>fgf :GFiles<CR> " For git files
nmap <leader>fb :Buffers<CR> " Search buffers
nmap <leader>frg :Rg 

