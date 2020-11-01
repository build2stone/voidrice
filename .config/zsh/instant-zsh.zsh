# Usage: instant-zsh-pre loading-prompt
#
# Prints the specified prompt after percent expansion (but not single word shell
# expansions). The prompt is replaced inplace once the real prompt is initialized.
#
# For best results, set loading-prompt to something that looks similar
# (or, ideally, identical) to the real prompt.
#
# Must be called at the very top of ~/.zshrc. Must be paired with instant-zsh-post.
#
# If you want to override PROMPT_EOL_MARK, do it before calling instant-zsh-pre.
function instant-zsh-pre() {
  zmodload zsh/terminfo

  # Do nothing if terminal is lacking required capabilities.
  (( ${+terminfo[cuu]} && ${+terminfo[ed]} && ${+terminfo[sc]} && ${+terminfo[rc]} )) || return 0

  unsetopt localoptions prompt_cr prompt_sp

  () {
    emulate -L zsh

    # Emulate prompt_cr and prompt_sp.
    local eol_mark=${PROMPT_EOL_MARK-"%B%S%#%s%b"}
    local -i fill=COLUMNS

    () {
      local COLUMNS=1024
      local -i x y=$#1 m
      if (( y )); then
        while (( ${${(%):-$1%$y(l.1.0)}[-1]} )); do
          echo $y
          x=y
          (( y *= 2 ));
        done
        local xy
        while (( y > x + 1 )); do
          m=$(( x + (y - x) / 2 ))
          typeset ${${(%):-$1%$m(l.x.y)}[-1]}=$m
        done
      fi
      (( fill -= x ))
    } $eol_mark

    print -r ${(%):-$eol_mark${(pl.$fill.. .)}$'\r'%b%k%f%E}$'\n\n\n\n\n\n\n\n\n'
    echoti cuu 10
    print -rn -- ${terminfo[sc]}${(%)1}

    _clear-loading-prompt() {
      unsetopt localoptions
      setopt prompt_cr prompt_sp
      () {
        emulate -L zsh
        # Clear the loading prompt. The real prompt is about to get printed.
        print -rn -- $terminfo[rc]$terminfo[sgr0]$terminfo[ed]
        unfunction _clear-loading-prompt
        precmd_functions=(${(@)precmd_functions:#_clear-loading-prompt})
      }
    }
    precmd_functions=($precmd_functions _clear-loading-prompt)
  } "$@"
}

# Must be called at the very bottom of ~/.zshrc. Must be paired with
# instant-zsh-pre.
function instant-zsh-post() {
  emulate -L zsh
  if (( ${+precmd_functions} && ${+precmd_functions[(I)_clear-loading-prompt]} )); then
    # Move _clear-loading-prompt to the end so that the loading prompt doesn't get
    # erased too soon. This assumes that the real prompt is set during the first
    # `precmd` or even earlier.
    precmd_functions=(${(@)precmd_functions:#_clear-loading-prompt} _clear-loading-prompt)
  fi
}
