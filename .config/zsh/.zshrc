source ~/.config/zsh/aliases

# Load colors
autoload -U colors && colors	

autoload -Uz compinit; compinit

# autocomplete
_comp_options+=(globdots) # With hidden files
source ~/.config/zsh/completion.zsh

# vi mode
bindkey -v
export KEYTIMEOUT=1

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

fpath=(~/.config/zsh $fpath)
autoload -Uz prompt; prompt
