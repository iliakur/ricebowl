# This works on both ubuntu and fedora, whereas placing it just in zshrc doesn't.
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh --no-rehash)"
export PATH="$HOME/.poetry/bin:$PATH"
