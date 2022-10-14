#!/usr/bin/env bash
# Software for entire team
brew install --cask docker
brew install --cask iterm2
# Personal software
brew install --cask emacs
brew install pyright
brew install shellcheck
brew install bashate
brew install fd
brew install ripgrep
brew install htop
HOMEBREW_CASK_OPTS="" brew install --cask dropbox
brew install --cask karabiner-elements
mkdir -p ~/.config/karabiner
ln -s --force "${PWD}"/karabiner.json ~/.config/karabiner
# m1 specific
if [ "$(arch)" = "arm64" ]; then
    echo "Running on M1 chip, additional setup needed."
    echo "Installing rosetta and pyenv-alias plugin..."
    softwareupdate --install-rosetta --agree-to-license
    git clone https://github.com/s1341/pyenv-alias.git $(pyenv root)/plugins/pyenv-alias
    echo "Install complete, follow these steps to finish setting things up:"
    echo "https://towardsdatascience.com/how-to-use-manage-multiple-python-versions-on-an-apple-silicon-m1-mac-d69ee6ed0250"
fi
