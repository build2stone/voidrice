let mapleader =","

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
Plug 'morhetz/gruvbox'
" modes
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'jreybert/vimagit'
" lf
Plug 'ptzz/lf.vim'
Plug 'voldikss/vim-floaterm'
" IDE
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf.vim'
Plug 'antoinemadec/coc-fzf'
Plug 'liuchengxu/vim-which-key'
" editing
Plug 'tmsvg/pear-tree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/argtextobj.vim'
Plug 'lukesmithxyz/vimling'
" niceties
Plug 'vim-airline/vim-airline'
Plug 'ap/vim-css-color'
Plug 'luochen1990/rainbow'
Plug 'machakann/vim-highlightedyank'
" syntax-highlighting
Plug 'daezak/crafttweaker-vim-highlighting'
Plug 'soli/prolog-vim'
Plug 'cespare/vim-toml'
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
set laststatus=0
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
	syntax on
	colorscheme gruvbox
" Removes background:
"	highlight Normal ctermbg=none
	set encoding=utf-8
	set number relativenumber
" Enable 24-bit color
	set termguicolors
" Enable autocompletion:
	set wildmode=longest,list,full
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Perform dot commands over visual blocks:
	vnoremap . :normal .<CR>
" Goyo plugin makes text more readable when writing prose:
	map <leader>f :Goyo \| set linebreak<CR>
" Spell-check set to <leader>o, 'o' for 'orthography':
	map <leader>o :setlocal spell! spelllang=en_us<CR>
" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow splitright

" airline
	let g:airline_theme='dark'

" fzf
    let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }

" highlightedyank
	let g:highlightedyank_highlight_duration = 200

" lf
	let g:lf_map_keys = 0
	map <leader>l :Lf<CR>

" pear-tree
	let g:pear_tree_smart_openers = 1
	let g:pear_tree_smart_closers = 1
	let g:pear_tree_smart_backspace = 1
	imap <space> <Plug>(PearTreeSpace)

" prolog-vim
    autocmd BufNewFile,BufRead *.pl set filetype=prolog

" rainbow
	let g:rainbow_active = 1

" vimling:
	nm <leader><leader>d :call ToggleDeadKeys()<CR>
	imap <leader><leader>d <esc>:call ToggleDeadKeys()<CR>a
	nm <leader><leader>i :call ToggleIPA()<CR>
	imap <leader><leader>i <esc>:call ToggleIPA()<CR>a
	nm <leader><leader>q :call ToggleProse()<CR>

" which-key
    nnoremap <silent> <Space>	:<c-u>WhichKey	'<Space>'<CR>
    nnoremap <silent> ,			:<c-u>WhichKey	','<CR>

" coc.nvim
    let g:coc_start_at_startup=0
	map <leader>e :call CocSetup() \| :CocStart<CR>

	function! CocSetup()
			" Old versions of gradle just straight up crash when there is unicode in the environment
			unlet $LF_ICONS

			" TextEdit might fail if hidden is not set.
			set hidden
			" Some servers have issues with backup files, see #649.
			set nobackup
			set nowritebackup
			" Give more space for displaying messages.
			set cmdheight=2
			" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
			" delays and poor user experience.
			set updatetime=300
			" Don't pass messages to |ins-completion-menu|.
			set shortmess+=c

			" Use tab for trigger completion with characters ahead and navigate.
			inoremap <silent><expr> <TAB>
									\ pumvisible() ? "\<C-n>" :
									\ <SID>check_back_space() ? "\<TAB>" :
									\ coc#refresh()
			inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

			function! s:check_back_space() abort
					let col = col('.') - 1
					return !col || getline('.')[col - 1]  =~# '\s'
			endfunction

			" Use <c-space> to trigger completion.
			inoremap <silent><expr> <c-space> coc#refresh()

			" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
			" position. Coc only does snippet and additional edit on confirm.
			if exists('*complete_info')
					imap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u<Plug>(PearTreeExpand)"
			else
					imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u<Plug>(PearTreeExpand)"
			endif

			" Use `[g` and `]g` to navigate diagnostics
			nmap <silent> [g <Plug>(coc-diagnostic-prev)
			nmap <silent> ]g <Plug>(coc-diagnostic-next)

			" GoTo code navigation.
			nmap <silent> gd <Plug>(coc-definition)
			nmap <silent> gy <Plug>(coc-type-definition)
			nmap <silent> gi <Plug>(coc-implementation)
			nmap <silent> gr <Plug>(coc-references)

			" Use K to show documentation in preview window.
			nnoremap <silent> K :call <SID>show_documentation()<CR>

			function! s:show_documentation()
					if (index(['vim','help'], &filetype) >= 0)
							execute 'h '.expand('<cword>')
					else
							call CocAction('doHover')
					endif
			endfunction

			" Highlight the symbol and its references when holding the cursor.
			autocmd CursorHold * silent call CocActionAsync('highlight')

			" Symbol renaming.
			nmap <leader>rn <Plug>(coc-rename)

			" Apply AutoFix to problem on the current line.
			nmap <leader>qf  <Plug>(coc-fix-current)

			" Formatting selected code.
			xmap <silent> <leader>f  <Plug>(coc-format-selected)
			nmap <silent> <leader>f  <Plug>(coc-format-selected)
			xmap <silent> <leader>a  :'<,'>CocAction<CR>
			nmap <silent> <leader>a  :'<,'>CocAction<CR>

			nnoremap <silent> <space>a  :<C-u>CocFzfList actions<CR>
			nnoremap <silent> <space>d  :<C-u>CocFzfList diagnostics<CR>
			nnoremap <silent> <space>b  :<C-u>CocFzfList diagnostics --current-buf<CR>
			nnoremap <silent> <space>c  :<C-u>CocFzfList commands<CR>
			nnoremap <silent> <space>e  :<C-u>CocFzfList extensions<CR>
			nnoremap <silent> <space>l  :<C-u>CocFzfList location<CR>
			nnoremap <silent> <space>o  :<C-u>CocFzfList outline<CR>
			nnoremap <silent> <space>s  :<C-u>CocFzfList symbols<CR>
			nnoremap <silent> <space>S  :<C-u>CocFzfList services<CR>
			nnoremap <silent> <space><tab>  :<C-u>CocFzfListResume<CR>

			" Map function and class text objects
			" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
			xmap if <Plug>(coc-funcobj-i)
			omap if <Plug>(coc-funcobj-i)
			xmap af <Plug>(coc-funcobj-a)
			omap af <Plug>(coc-funcobj-a)
			xmap ic <Plug>(coc-classobj-i)
			omap ic <Plug>(coc-classobj-i)
			xmap ac <Plug>(coc-classobj-a)
			omap ac <Plug>(coc-classobj-a)
	endfunction

" Shortcutting split navigation, saving a keypress:
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l

" Replace ex mode with gq
	map Q gq

" Check file in shellcheck:
	map <leader>s :!clear && shellcheck -x %<CR>

" Open my bibliography file in split
	map <leader>b :vsp<space>$BIB<CR>
	map <leader>r :vsp<space>$REFER<CR>

" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
	map <leader>c :w! \| !compiler "<c-r>%"<CR>

" Open corresponding .pdf/.html or preview
	map <leader>p :!opout <c-r>%<CR><CR>

" Runs a script that cleans out tex build files whenever I close out of a .tex file.
	autocmd VimLeave *.tex !texclear %

" Ensure files are read as what I want:
	let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
	map <leader>v :VimwikiIndex
	let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
	autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile *.tex set filetype=tex

" Save file as sudo on files that require root permission
	cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Enable Goyo by default for mutt writing
	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo | set bg=dark
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<CR>
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZQ :Goyo\|q!<CR>

" Automatically deletes all trailing whitespace and newlines at end of file on save.
	autocmd BufWritePre * %s/\s\+$//e
	autocmd BufWritePre * %s/\n\+\%$//e
	autocmd BufWritePre *.[ch] %s/\%$/\r/e

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

" Function for toggling the bottom statusbar:
let s:hidden_all = 0
function! ToggleHiddenAll()
    if s:hidden_all  == 0
        let s:hidden_all = 1
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
    else
        let s:hidden_all = 0
        set showmode
        set ruler
        set laststatus=2
        set showcmd
    endif
endfunction
nnoremap <leader>h :call ToggleHiddenAll()<CR>
