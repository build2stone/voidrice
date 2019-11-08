# Makes zsh's vi-mode use xclip

function vi-put-after-xclip {
	CUTBUFFER=$(xclip -selection clipboard -out)
	zle vi-put-after
}
zle -N vi-put-after-xclip
bindkey -M vicmd 'P' vi-put-after-xclip

function vi-put-before-xclip {
	CUTBUFFER=$(xclip -selection clipboard -out)
	zle vi-put-before
}
zle -N vi-put-before-xclip
bindkey -M vicmd 'p' vi-put-before-xclip

function vi-yank-xclip {
	zle vi-yank
	echo "$CUTBUFFER" | xclip -selection clipboard
}
zle -N vi-yank-xclip
bindkey -M vicmd 'y' vi-yank-xclip

