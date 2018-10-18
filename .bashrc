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
alias astud='/home/colton/downloads/android-studio/bin/studio.sh' #android studio
# eclimd alias; needs constant updating (find ~/.eclipse | grep eclimd)
alias eclimd='~/.eclipse/org.eclipse.platform_4.7.3_155965261_linux_gtk_x86_64/eclimd'
alias scussh='ssh cpowell@linux.dc.engr.scu.edu'
alias pak='pulseaudio -k'

#Python 
alias myvenv='python3 -m venv ./venv' #Creates a ven`v in local dir
alias myact='source ./venv/bin/activate'



# Note: gcc/g++ refers to current versions of GNU C/C++ compiler, while gcc-6/g++-6 refers to version 6.4.1 of the compilers (used for CUDA+Tensorflow)
alias gtags='gtags --gtagslabel=new-ctags'

function firefox() {
    echo "Firefox should not be executed from CLI."
}

function chromium() {
    echo "Chromium should not be executed from CLI."
}

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

export GTAGSLIBPATH=$HOME/.gtags/:/usr/include/
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=new-ctags

# for vlc and good-looking qt5 apps (qt5ct relies on this)
export QT_QPA_PLATFORMTHEME=qt5ct

# tensorflow stoof (NO LONGER NEEDED?)
# export PATH="/opt/cuda/bin:$PATH"

# export LD_LIBRARY_PATH=/opt/cuda/lib64\
# ${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}

# export LD_LIBRARY_PATH="/opt/cuda/lib64:$LD_LIBRARY_PATH"
# export LD_LIBRARY_PATH="/home/colton/downloads/cuda/lib64:$LD_LIBRARY_PATH"
# export CUDA_HOME=/opt/cuda

PS1='[\u@\h \W]\$ '

# Comment this out to increase terminal load times:
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
