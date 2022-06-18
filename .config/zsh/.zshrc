if [[ "$TERM" == "dumb" ]]; then
    unset zle_bracketed_paste
    unset zle
    PS1='%Bλ%b: '
    return
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# wal colors
(cat ~/.cache/wal/sequences)

source ~/.config/zsh/aliases

HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/.zsh_history

PS1='%Bλ%b: '

source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
