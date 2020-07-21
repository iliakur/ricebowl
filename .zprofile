#-> ~/{name}
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh --no-rehash)"

export PATH="$HOME/.cargo/bin:$PATH"
