call plug#begin('~/.config/nvim/plugins')
Plug 'bkad/CamelCaseMotion'
Plug 'chrisbra/Colorizer'
Plug 'chriskempson/base16-vim'
Plug 'dense-analysis/ale'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'jremmen/vim-ripgrep'
Plug 'morhetz/gruvbox'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neovimhaskell/haskell-vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'qpkorr/vim-bufkill'
Plug 'roryokane/detectindent'
Plug 'tomasiser/vim-code-dark'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vhdirk/vim-cmake'
Plug 'vim-airline/vim-airline'

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#ale#enabled = 1
let g:airline_theme = "gruvbox"
let g:airline_symbols_ascii = 1
let g:airline_section_a = "%{airline#util#wrap(airline#parts#mode(),0)}%#__restore__#%{airline#util#append(airline#parts#crypt(),0)}%{airline#util#append(airline#parts#paste(),0)}%{airline#util#append(airline#extensions#keymap#status(),0)}%{airline#util#append(airline#parts#spell(),0)}%{airline#util#append(\"\",0)}%{airline#util#append(\"\",0)}%{airline#util#append(airline#parts#iminsert(),0)}"
let g:airline_section_z = "line %l/%L:col %v"
let g:ale_linters = {
            \ 'c': ['clangtidy'],
            \ 'cpp': ['clangtidy'],
            \ }
let g:ale_c_clangtidy_checks = ['bugprone-*', 'performance-*', '-clang-analyzer-security.*']
let g:ale_cpp_clangtidy_checks = ['bugprone-*', 'performance-*', '-clang-analyzer-security.*']
let g:ale_linters_explicit = 1
let g:ale_sign_warning = "⚠️"
let g:camelcasemotion_key = "<leader>"
let g:cmake_cxx_compiler = "/usr/bin/clang++"
let g:cmake_c_compiler = "/usr/bin/clang"
let g:cmake_export_compile_commands = 1
let g:detectindent_preferred_indent = 4
let g:gruvbox_contrast_dark = "hard"
call plug#end()

colorscheme gruvbox
highlight link ALEErrorSign CocErrorSign
highlight link ALEWarningSign CocWarningSign

set termguicolors
set mouse=a
set number relativenumber
set updatetime=300
set softtabstop=4 shiftwidth=4 tabstop=4 expandtab smartindent copyindent
set noshowmode
set undofile undodir=~/.config/nvim/undo
set hidden
set completeopt=menu,menuone,longest
set notimeout
set ignorecase smartcase
set listchars+=tab:>\  list
set cursorline
set shortmess+=c
set signcolumn=yes
set foldmethod=syntax foldlevelstart=99

let mapleader = " "

com! -nargs=1 LitSearch :let @/='\V'.escape(<q-args>, '\\')| normal! n
autocmd FileType c,cpp,objc setlocal commentstring=//%s
nnoremap <leader>/ :LitSearch 

nnoremap _ "_
inoremap <silent> <c-s> <esc>:w<cr>
inoremap <silent> <c-q> <esc>:wa<cr>
nnoremap <silent> <leader><space> :Files<cr>
nnoremap <silent> <esc> :noh<cr>
nnoremap <silent> <leader>di :DetectIndent<cr>
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
inoremap <silent><expr> <c-space> coc#refresh()
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
            \: "\<C-g>u\<cr>\<c-r>=coc#on_enter()\<cr>"
nmap <silent> [g <plug>(coc-diagnostic-prev)
nmap <silent> ]g <plug>(coc-diagnostic-next)
nmap <silent> gd <plug>(coc-definition)
nnoremap <silent> <leader>cd :CocDiagnostics<cr>
nmap <silent> gy <plug>(coc-type-definition)
nmap <silent> gi <plug>(coc-implementation)
nmap <silent> gr <plug>(coc-references)
nnoremap <silent> K :call <sid>show_documentation()<cr>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
autocmd CursorHold * silent call CocActionAsync('highlight')
nmap <leader>rn <plug>(coc-rename)

autocmd FileType haskell set ts=3 sts=3 sw=3
