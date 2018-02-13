#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# alias list
alias ls='ls --color=auto'
alias em='emacs'
alias enx='emacs -nw' #emacs nox
alias enl='emacs -Q -nw --load "~/.emacs-quick"' #emacs noload(quick)
alias ssa='xscreensaver-command -activate'
alias mann='/usr/bin/man'

# eclimd alias; needs constant updating
alias eclimd='~/.eclipse/org.eclipse.platform_4.7.1_155965261_linux_gtk_x86_64/eclimd'

# # check that a man page exists
# function man() {
#     cmd='enl'
#     found_pages=0

#     for((i=1;i<=$#;i++)); do
# 	# Get return code for man "arg-i", silence output
#         ( /usr/bin/man ${!i} & wait $! ) > /dev/null 2>&1

# 	# if the page exists add it to our command
# 	if [ $? -eq 0 ]; then
#      	    cmd+=' -eval "(man \"'${!i}'\")"'
# 	    ((found_pages++))
# 	else
# 	    echo 'No manual entry for' ${!i}
# 	fi
	
#     done

#     # Delete scratch/splash/etc window after opening man pages
#     cmd+=' -eval "(delete-window)"'
 
#     # If no pages are found then don't exec
#     if [ $found_pages -gt 0 ]; then
# 	eval $cmd
#     else
# 	echo 'No manual entries found. Exiting.'
#     fi
	
#     unset cmd
# }

# tensorflow stoof
export PATH="/opt/cuda/bin:$PATH"
#export LD_LIBRARY_PATH=/opt/cuda/lib64\
#       ${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
export LD_LIBRARY_PATH="/opt/cuda/lib64:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="/home/colton/downloads/cuda/lib64:$LD_LIBRARY_PATH"
export CUDA_HOME=/opt/cuda
export GTAGSLIBPATH=$HOME/.gtags/
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=new-ctags

PS1='[\u@\h \W]\$ '
