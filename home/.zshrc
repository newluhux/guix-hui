# plugin
autoload -Uz compinit promptinit
compinit
promptinit

prompt redhat
zstyle ':completion:*' menu select

# alias
alias ustardict='ustardict 
	~/.guix-home/profile/share/stardict/stardict-ecdict-2.4.2.'
