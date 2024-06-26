# Adapted from Luke's config for the Zoomer Shell

source "${ZDOTDIR}/instant-zsh.zsh"

# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
echo -ne '\e[5 q' # Use beam shape cursor on startup.
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
instant-zsh-pre "$PS1"
setopt autocd autopushd pushdignoredups # Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# History
HISTSIZE=1000000000
SAVEHIST=1000000000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"
setopt inc_append_history # Record history immediately
setopt hist_ignore_dups # Skip immediate duplicates
setopt hist_reduce_blanks # Remove unnecessary blanks
setopt hist_ignore_space  # Skip entries with leading space
setopt hist_fcntl_lock # Use fcntl to lock HISTFILE

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"

# Plugins
export ZSH_AUTOSUGGEST_USE_ASYNC=t
export ZSH_AUTOSUGGEST_MANUAL_REBIND=t
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

source "${ZDOTDIR}/clipboard.zsh"
source "${ZDOTDIR}/plugins.zsh"

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*' group-name ''
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.
setopt numeric_glob_sort

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp -uq)"
    trap 'rm -f $tmp >/dev/null 2>&1 && trap - HUP INT QUIT TERM PWR EXIT' HUP INT QUIT TERM PWR EXIT
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' '^ulfcd\n'

# Accept autosuggestion with alt-tab
bindkey '\e\t' autosuggest-accept

# Substring history search with shift+up/down
bindkey '^[[1;2A' history-substring-search-up
bindkey '^[[1;2B' history-substring-search-down
bindkey -M vicmd 'K' history-substring-search-up
bindkey -M vicmd 'J' history-substring-search-down

bindkey -s '^a' '^ubc -lq\n'

bindkey -s '^f' '^ucd "$(dirname "$(fzf)")"\n'

# Bind Insert, Delete, Home and End
bindkey "^[[4h" vi-put-after
bindkey '^[[P' delete-char
bindkey "^[[4~" end-of-line
bindkey "^[[H" beginning-of-line

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -M vicmd '^[[P' vi-delete-char
bindkey -M vicmd '^e' edit-command-line
bindkey -M visual '^[[P' vi-delete

instant-zsh-post
