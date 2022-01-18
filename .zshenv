export PATH="$HOME/workspace/ripgrep/target/release:$PATH"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

export ZSH=$HOME/.oh-my-zsh

export DISABLE_CORRECTION="true"
plugins=(git)

export GOROOT=/usr/local/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
export PATH=$HOME/go/bin:$PATH

source ~/.exports
source ~/.functions
source ~/.aliases
source ~/.private
source ~/bin/z/z.sh
export _Z_CMD=j

setxkbmap -option "caps:swapescape"

source "$HOME/.cargo/env"

if [ -e /home/brady/.nix-profile/etc/profile.d/nix.sh ]; then . /home/brady/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export PATH="$PATH:$HOME/snap/flutter/common/flutter/bin"

export PATH="$PATH:$HOME/.ghcup/bin"
source "$HOME/.ghcup/env"
