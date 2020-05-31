let mapleader =","

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
Plug 'morhetz/gruvbox'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'jreybert/vimagit'
Plug 'lukesmithxyz/vimling'
Plug 'vimwiki/vimwiki'
Plug 'bling/vim-airline'
Plug 'tpope/vim-commentary'
Plug 'ptzz/lf.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'kovetskiy/sxhkd-vim'
Plug 'ap/vim-css-color'
Plug 'tmsvg/pear-tree'
Plug 'machakann/vim-highlightedyank'
Plug 'soli/prolog-vim'
Plug 'cespare/vim-toml'
Plug 'jaxbot/semantic-highlight.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

set bg=dark
set tabstop=4
set go=a
set mouse=a
set clipboard+=unnamedplus

" Text selected with mouse goes to primary selection
set mouse=a
vmap <LeftRelease> "*ygv

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

" Goyo plugin makes text more readable when writing prose:
	map <leader>f :Goyo \| set linebreak<CR>

" Spell-check set to <leader>o, 'o' for 'orthography':
	map <leader>o :setlocal spell! spelllang=en_us<CR>

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow splitright

" airline
	let g:airline_theme='dark'
" lf
	let g:lf_map_keys = 0
	map <leader>l :Lf<CR>

" Nerd tree
	map <leader>n :NERDTreeToggle<CR>
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
    if has('nvim')
        let NERDTreeBookmarksFile = stdpath('data') . '/NERDTreeBookmarks'
    else
        let NERDTreeBookmarksFile = '~/.vim' . '/NERDTreeBookmarks'
    endif

" pear-tree
	let g:pear_tree_smart_openers = 1
	let g:pear_tree_smart_closers = 1
	let g:pear_tree_smart_backspace = 1
	imap <space> <Plug>(PearTreeSpace)

" vimling:
	nm <leader>d :call ToggleDeadKeys()<CR>
	imap <leader>d <esc>:call ToggleDeadKeys()<CR>a
	nm <leader>i :call ToggleIPA()<CR>
	imap <leader>i <esc>:call ToggleIPA()<CR>a
	nm <leader>q :call ToggleProse()<CR>

" highlightedyank
	let g:highlightedyank_highlight_duration = 200

" semantic-highlight
	nnoremap <Leader>h :SemanticHighlightToggle<cr>

" coc.nvim
    let g:coc_start_at_startup=0
	map <leader>e :call CocSetup() \| :CocStart<CR>

	function! CocSetup()
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

			" Formatting selected code.
			xmap <leader>f  <Plug>(coc-format-selected)
			nmap <leader>f  <Plug>(coc-format-selected)
			xmap <leader>a  <Plug>(coc-codeaction-selected)
			nmap <leader>a  <Plug>(coc-codeaction-selected)

			" Remap keys for applying codeAction to the current buffer.
			nmap <leader>ac  <Plug>(coc-codeaction)
			" Apply AutoFix to problem on the current line.
			nmap <leader>qf  <Plug>(coc-fix-current)

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
	map <leader>s :!clear && shellcheck %<CR>

" Open my bibliography file in split
	map <leader>b :vsp<space>$BIB<CR>
	map <leader>r :vsp<space>$REFER<CR>

" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
	map <leader>c :w! \| !compiler <c-r>%<CR>

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

" Enable Goyo by default for mutt writting
	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo | set bg=dark
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<CR>
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZQ :Goyo\|q!<CR>

" Automatically deletes all trailing whitespace and newlines at end of file on save.
	autocmd BufWritePre * %s/\s\+$//e
	autocmd BufWritepre * %s/\n\+\%$//e

" When shortcut files are updated, renew bash and ranger configs with new material:
	autocmd BufWritePost files,directories !shortcuts
" Run xrdb whenever Xdefaults or Xresources are updated.
	autocmd BufWritePost *Xresources,*Xdefaults !xrdb %
" Update binds when sxhkdrc is updated.
	autocmd BufWritePost *sxhkdrc !pkill -USR1 sxhkd
" Auto-compile zsh files
	autocmd BufWritePost *.zsh !zcompile %

" Turns off highlighting on the bits of code that are changed, so the line that is changed is highlighted but the actual text that has changed stands out on the line and is readable.
if &diff
    highlight! link DiffText MatchParen
endif
