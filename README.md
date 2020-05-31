# DOTFILES

Repository containing my personal dotfiles.

## Cloning the repository

``` sh
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'\
git clone --bare https://github.com/runeanielsen/dotfiles.git $HOME/.dotfiles\
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'\
config checkout
```

## Getting updated

``` sh
config pull
```
