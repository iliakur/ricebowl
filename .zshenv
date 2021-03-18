#-> ~/{name}
# Path modifications should be available to both login and non-login shells.
export PATH="$HOME/.cargo/bin:$PATH"
# pyenv settings
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - --no-rehash zsh)"

# System-wide Poetry
export PATH="$HOME/.poetry/bin:$PATH"
