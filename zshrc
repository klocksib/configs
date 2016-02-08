# The following lines were added by compinstall
zstyle :compinstall filename '/home/bklocksiem/.zshrc'

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz colors
colors

prompt adam1

# zsh-completions package
fpath=(/usr/local/share/zsh-completions $fpath)

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.history

# between quotation marks is the tool output for LS_COLORS
#export LS_COLORS="di=31;41:ln=31;41:so=31;41:pi=31;41:ex=31;41:bd=31;41:cd=31;41:su=31;41:sg=31;41:tw=31;41:ow=31;41:"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}


export EDITOR="emacs -nw"
export SVN_EDITOR=$EDITOR

# End of lines added by compinstall

#####################################################################
## Customization
#####################################################################
SYSTEMTYPE=`uname`

alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias e="emacsclient -c"
bindkey '^?' backward-delete-char
bindkey '^[[3~' delete-char

if [[ ${SYSTEMTYPE} == "Darwin" ]]; then
    export PATH=/usr/local/bin:$PATH
    export PATH=/usr/local/sbin:$PATH
    export MONO_GAC_PREFIX=/usr/local
    
    alias ls="ls -G"
    alias s="/usr/local/bin/svn"
    alias svn="/usr/local/bin/svn"
    alias svn17="/usr/bin/svn"

    source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

    source ~/perl5/perlbrew/etc/bashrc
    perlbrew use 5.20.1

    # syntax-highlighting package
    source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
