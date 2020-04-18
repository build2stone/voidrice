# Adapted from Luke's config for the Zoomer Shell

# zplugin module setup
module_path+=( "/home/seggers/.config/zinit/bin/zmodules/Src" )
zmodload zdharma/zplugin

# This makes the prompt appear instantly (using instant-zsh-pre/post)
source "$HOME/.local/zshscripts/instant-zsh.zsh"

# zplugin setup
declare -A ZINIT
ZINIT[HOME_DIR]="$HOME/.config/zinit"
ZINIT[ZCOMPDUMP_PATH]="$HOME/.cache/zcompdump"
source "$HOME/.config/zinit/bin/zplugin.zsh"

# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
setopt autocd autopushd pushdignoredups # Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
instant-zsh-pre "$PS1"

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE="$XDG_CACHE_HOME/zsh/history"
[ -e "$XDG_CACHE_HOME/zsh/" ] || mkdir "$XDG_CACHE_HOME/zsh/"

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/zshnameddirrc"

# Load pty.zsh for piping colored output
# (Makes commands think stdout is a tty)
source "$HOME/.local/zshscripts/pty.zsh"

# Load plugins
zplugin light zsh-users/zsh-autosuggestions
zplugin light hlissner/zsh-autopair
zplugin light zsh-vi-more/vi-motions
zplugin light zsh-users/zsh-history-substring-search
zplugin light zsh-users/zsh-syntax-highlighting

# Set zsh-autosuggestions options
export ZSH_AUTOSUGGEST_USE_ASYNC=t
export ZSH_AUTOSUGGEST_MANUAL_REBIND=t
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*' group-name ''
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

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
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lfifo -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' 'lfcd\n'

# Accept autosuggestion with alt-tab
bindkey '\e\t' autosuggest-accept

# Substring history search with shift+up/down
bindkey '^[[1;2A' history-substring-search-up
bindkey '^[[1;2B' history-substring-search-down
bindkey -M vicmd 'K' history-substring-search-up
bindkey -M vicmd 'J' history-substring-search-down

bindkey -s '^a' 'bc -l\n'

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

# Bind Insert, Delete, Home and End to the expected
bindkey "^[[4h" vi-put-after-xclip
bindkey '^[[P' delete-char
bindkey "^[[4~" end-of-line
bindkey "^[[H" beginning-of-line

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

instant-zsh-post
