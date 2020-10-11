#-*-shell-script-*-
#-> ~/.{name}

# Stuff for work

# Kubectl
source <(kubectl completion zsh)
alias k=kubectl
complete -F __start_kubectl k

# NVM setup
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias mytest="pytest --no-cov-on-fail"

function k8s-login(){
    $HOME/code/k8s-rtr/scripts/k8s-dev-login.sh $(cat $HOME/.rtr-credentials)
}

# This saves some typing when having to move data from one pod to another
# as part of our data pipeline.
function move-embedded-to-ml-shell(){
    echo "Downloading file"
    kubectl cp \
            -n rtr-machine-learning \
            rtr-machine-learning36-02-79ccf84679-8tfwp:/home/makrutat/variant-suggestion-data/$1 \
            $HOME/Downloads/$1
    echo "Done downloading, starting upload."
    kubectl cp \
            -n rtr-machine-learning \
            $HOME/Downloads/$1 \
            ml-shell-59c4f5f9bd-4txxn:/home/makrutat/variant-suggestion-data/$1
    echo "Done uploading!"
}

# Copy local file to polyaxon upload shell.
function upload-to-plx(){
    kubectl cp \
            -n polyaxon \
            $1 \
            polyaxon-upload-shell-0:/plx-data/datasets/variants-suggestion/$1
}

alias piptoolz="pip install pytest-xdist pytest-sugar ipdb"

# pipx
# Created by `userpath` on 2020-06-29 04:38:45
export PATH="$PATH:/home/ilia/.local/bin"
