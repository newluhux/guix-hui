# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
[ -f /etc/bashrc ] && source /etc/bashrc


# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env]\$ '
else
    PS1='\u@\h \w\$ '
fi

# path
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

# stardict
export STARDICT_INDEX=/sdcard/dict/stardict-ecdict-2.4.2/stardict-ecdict-2.4.2.idx
export STARDICT_DICT=/sdcard/dict/stardict-ecdict-2.4.2/stardict-ecdict-2.4.2.dict

# editor
export EDITOR=vi

# graphic
if [ $(tty) == "/dev/tty1" ]
then
	if [ $(id -u) == "1000" ]
	then
		exec hikari
	fi
fi