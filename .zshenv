# export PYENV_ROOT="$HOME/.pyenv"
# export JAVA_HOME="/usr/bin/"
# export PATH="$PYENV_ROOT/bin:$PATH"
# export PATH="$HOME/.rbenv/bin:$PATH"
# export PATH="$HOME/.cargo/bin:$PATH"
# eval "$(rbenv init -)"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

export ZSH=$HOME/.oh-my-zsh

export DISABLE_CORRECTION="true"
plugins=(git)

# OPAM configuration
# . /home/tippenein/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
# Path to your oh-my-zsh configuration.

export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"

export GOROOT=/usr/local/go 
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

source ~/.exports
source ~/.functions
source ~/.aliases
source ~/.private
source ~/bin/z/z.sh
export _Z_CMD=j


# eval "$(ssh-agent -s)"
# ssh-add

setxkbmap -option "caps:swapescape"
source "$HOME/.cargo/env"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/brady/.miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/brady/.miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/brady/.miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/brady/.miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup

