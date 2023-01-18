# DOTFILES

Repository containing my personal dotfiles.

## Cloning the repository

``` sh
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'\
git clone --bare git@github.com:runeanielsen/dotfiles.git $HOME/.dotfiles\
dotfiles checkout
```

To not show untracked files
```sh
dotfiles config status.showUntrackedFiles no
```

## Getting updated

``` sh
dotfiles pull
```
