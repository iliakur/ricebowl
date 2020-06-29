#-*-shell-script-*-
#-> ~/.{name}

# Stuff for home

# Aliases for working with NLTK
alias nltkdev='cd ~/code/python/nltk'
alias mut='python -m pytest --cov=nltk/lm --cov-report term-missing nltk/test/unit/lm'
alias mdt='python -m pytest --cov=nltk/lm --cov-report term-missing --doctest-modules nltk/lm'
alias mrt='python -m pytest --doctest-glob="*.doctest" nltk/test/lm.doctest'
alias mat='tox -e py37 -e py36 nltk.test.unit.lm lm.doctest'

backup () {
    # Rsync options:
    # -a		general archiving, useful
    # -v		need to explicitly check the results at first
    # --delete	delete files in dest which are missing in src
    # -L  turn symlinks into referent files/folders
    rsync -aLkv --delete \
          --exclude=".git" \
          --exclude=".venv" \
          --exclude="__pycache__" \
          ~/code/configs \
          ~/code/cogsys/ms-thesis \
          ~/Documents \
          /run/media/quickbeam/PLINKETT/backup/ \
        }

# Pipx
# Created by `userpath` on 2020-06-27 12:01:20
export PATH="$PATH:/home/quickbeam/.local/bin"
