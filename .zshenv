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
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
. "$HOME/.cargo/env"
