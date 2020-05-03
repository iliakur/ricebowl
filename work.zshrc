#-*-shell-script-*-
#-> ~/.{name}

# Stuff for work

# Kubectl
source <(kubectl completion zsh)
alias k=kubectl
complete -F __start_kubectl k
