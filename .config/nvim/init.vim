let mapleader =","

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
    echo "Downloading junegunn/vim-plug to manage plugins..."
    silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
" theme
Plug 'morhetz/gruvbox'
" modes
Plug 'jreybert/vimagit'
" editing
Plug 'tmsvg/pear-tree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/argtextobj.vim'
" niceties
Plug 'ap/vim-css-color'
Plug 'liuchengxu/vim-which-key'
Plug 'luochen1990/rainbow'
Plug 'machakann/vim-highlightedyank'
Plug 'nvim-lualine/lualine.nvim'
Plug 'tpope/vim-sleuth'
" syntax-highlighting
Plug 'cespare/vim-toml'
Plug 'daezak/crafttweaker-vim-highlighting'
Plug 'hjson/vim-hjson'
Plug 'lnl7/vim-nix'
call plug#end()

set title
set bg=dark
set tabstop=4
set shiftwidth=4
set go=a
set mouse=a
set clipboard+=unnamedplus
set noshowmode
set noruler
set laststatus=2
set noshowcmd

" Text selected with mouse goes to primary selection
set mouse=a
vmap <LeftRelease> "*ygv
" Un-highlight search terms with <Esc>
nmap <silent> <esc> :noh<cr>
" Some basics:
nnoremap c "_c
set nocompatible
filetype plugin on
colorscheme gruvbox
set number relativenumber
" Enable 24-bit color
set termguicolors
" Enable autocompletion:
set wildmode=longest,list,full
" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Perform dot commands over visual blocks:
vnoremap . :normal .<CR>
" Spell-check set to <leader>o, 'o' for 'orthography':
map <leader>o :setlocal spell! spelllang=en_us<CR>
" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
set splitbelow splitright

" highlightedyank
let g:highlightedyank_highlight_duration = 200

" lualine
lua << END
require'lualine'.setup {
    options = {
	theme = 'gruvbox',
	section_separators = '',
	component_separators = { left = '', right = ''}
	}
    }
END

" pear-tree
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1
imap <space> <Plug>(PearTreeSpace)

" rainbow
let g:rainbow_active = 1

" which-key
nnoremap <silent> <Space> :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> , :<c-u>WhichKey ','<CR>

" Shortcutting split navigation, saving a keypress:
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Replace ex mode with gq
map Q gq

" Check file in shellcheck:
map <leader>s :!clear && shellcheck -x %<CR>

" Replace all is aliased to S.
nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
map <leader>c :w! \| !compiler "<c-r>%"<CR>

" Open corresponding .pdf/.html or preview
map <leader>p :!opout <c-r>%<CR><CR>

" Runs a script that cleans out tex build files whenever I close out of a .tex file.
autocmd VimLeave *.tex !texclear %

" Ensure files are read as what I want:
autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
autocmd BufRead,BufNewFile *.tex set filetype=tex

" Save file as sudo on files that require root permission
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Automatically deletes all trailing whitespace and newlines at end of file on save. & reset cursor position
autocmd BufWritePre * let currPos = getpos(".")
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre * %s/\n\+\%$//e
autocmd BufWritePre *.[ch] %s/\%$/\r/e
autocmd BufWritePre * cal cursor(currPos[1], currPos[2])

" When shortcut files are updated, renew bash and ranger configs with new material:
autocmd BufWritePost bm-files,bm-dirs !shortcuts
" Run xrdb whenever Xdefaults or Xresources are updated.
autocmd BufRead,BufNewFile Xresources,Xdefaults,xresources,xdefaults set filetype=xdefaults
autocmd BufWritePost Xresources,Xdefaults,xresources,xdefaults !xrdb %
" Recompile dwmblocks on config edit.
autocmd BufWritePost ~/.local/src/dwmblocks/config.h !cd ~/.local/src/dwmblocks/; sudo make install && { killall -q dwmblocks;setsid -f dwmblocks }
" Auto-compile zsh files
autocmd BufWritePost *.zsh !zcompile %

" Turns off highlighting on the bits of code that are changed, so the line that is changed is highlighted but the actual text that has changed stands out on the line and is readable.
if &diff
    highlight! link DiffText MatchParen
endif

" Load command shortcuts generated from bm-dirs and bm-files via shortcuts script.
" Here leader is ";".
" So ":vs ;cfz" will expand into ":vs /home/<user>/.config/zsh/.zshrc"
" if typed fast without the timeout.
source ~/.config/nvim/shortcuts.vim
