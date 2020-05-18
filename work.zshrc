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

alias k8s-login="$HOME/code/k8s-rtr/scripts/k8s-dev-login.sh $(cat $HOME/.rtr-credentials)"
