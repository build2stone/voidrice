" This was adapted from https://github.com/vifm/vifm/blob/master/data/vim/ftplugin/vifm-rename.vim
" It was originally written by xaizek <xaizek@posteo.net>

" Closes window/tab/Vim when buffer is left alone in there
function! s:QuitIfOnlyWindow()
	" Boil out if there is more than one window
	if winbufnr(2) != -1
		return
	endif

	" Just close tab with this single window or quit Vim with last tab
	if tabpagenr('$') == 1
		bdelete
		quit
	else
		close
	endif
endfunction

wincmd w
setlocal buftype=nofile
setlocal bufhidden=hide
setlocal noswapfile
setlocal nobuflisted
setlocal cursorbind
setlocal scrollbind
setlocal nocursorline

" Setup a hook in auxiliary local window to do not leave it alone, when it's
" useless
augroup VifmRenameAutoCmds
	autocmd! * <buffer>
	autocmd BufEnter <buffer> call s:QuitIfOnlyWindow()
augroup END

windo e ++enc=utf-8

" Go back to the original window and ensure it will remain synchronized with
" the auxiliary one
wincmd w
setlocal cursorbind
setlocal scrollbind

set noro

" vim: set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab cinoptions-=(0 :
