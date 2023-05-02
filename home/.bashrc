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

# color
alias 'ls'='ls --color'

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

# pager
export PAGER='less -R'

# graphic
export WLR_NO_HARDWARE_CURSORS=1
export GDK_BACKEND=wayland
export QT_QPA_PLATFORM=wayland
export SDL_VIDEODRIVER=wayland
if [ $(tty) == "/dev/tty1" ]
then
	if [ $(id -u) == "1000" ]
	then
		exec weston
	fi
fi
