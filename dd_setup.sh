#!/usr/bin/env bash
brew update
# Don't update brew every time you install.
export HOMEBREW_NO_AUTO_UPDATE=1
# Team-relevant software
brew install --cask docker
brew install --cask iterm2

# Directory and git settings for personal programming projects.
# TODO make this idempotent
mkdir -vp "${HOME}"/src
cat <<EOT > "${HOME}"/src/.gitconfig
[user]
	email = ilia.kurenkov@gmail.com
EOT
cat <<EOT >> "${HOME}"/.gitconfig
[includeIf "gitdir:~/src/"]
    path = ~/src/.gitconfig
EOT

# Personal software
brew install --cask emacs
ln --symbolic --verbose "${PWD}"/spacemacs.d "${HOME}"/.spacemacs.d

# Fonts (mostly for emacs)
brew tap -v homebrew/cask-fonts
HOMEBREW_CASK_OPTS="" brew install font-inconsolata font-linux-biolinum

brew install pyright
brew install shellcheck
brew install bashate
brew install fd
brew install watch
brew install ripgrep
brew install htop
HOMEBREW_CASK_OPTS="" brew install --cask dropbox
brew install --cask karabiner-elements
mkdir -p ~/.config/karabiner
ln --symbolic --verbose --force "${PWD}"/karabiner.json "${HOME}"/.config/karabiner
# m1 specific
if [ "$(arch)" = "arm64" ]; then
    echo "Running on M1 chip, additional setup needed."
    echo "Installing rosetta and pyenv-alias plugin..."
    softwareupdate --install-rosetta --agree-to-license
    git clone https://github.com/s1341/pyenv-alias.git "$(pyenv root)"/plugins/pyenv-alias
    echo "Install complete, follow these steps to finish setting things up:"
    echo "https://towardsdatascience.com/how-to-use-manage-multiple-python-versions-on-an-apple-silicon-m1-mac-d69ee6ed0250"
fi
