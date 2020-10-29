source ~/.config/zsh/aliases

# load colors
autoload -U colors && colors 
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b"

# prompt
fpath=(~/.config/zsh $fpath)
autoload -Uz prompt; prompt

# history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# autocomplete
autoload -Uz compinit; compinit

_comp_options+=(globdots)
source ~/.config/zsh/completion.zsh

# vi mode
bindkey -v
export KEYTIMEOUT=1

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

source ~/.config/zsh/plugins/zsh-completions/zsh-completions.plugin.zsh
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
