# DOTFILES

Repository containing my personal dotfiles.

## Cloning the repository

``` sh
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'\
git clone --bare https://github.com/runeanielsen/dotfiles.git $HOME/.dotfiles\
config checkout
```

To not show untracked files
```sh
config config --global status.showUntrackedFiles no
```

## Getting updated

``` sh
config pull
```
