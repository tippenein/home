export ZSH=$HOME/.oh-my-zsh

export DISABLE_CORRECTION="true"
plugins=()

source ~/.exports
source ~/.functions
source ~/.aliases
source ~/.private
source ~/bin/z/z.sh

setxkbmap -option "caps:swapescape"

source "$HOME/.cargo/env"

if [ -e /home/brady/.nix-profile/etc/profile.d/nix.sh ]; then . /home/brady/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

source "$HOME/.ghcup/env"
