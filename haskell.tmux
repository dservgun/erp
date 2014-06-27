tmux has-session -t development
if [ $? != 0 ]
then
	#A tmux session for haskell
	#From the pragmatic programmmers tmux reference
	#Create a new session
	tmux new-session -s development -n editor -d
	# Changing into erp
	tmux send-keys -t development 'cd ~/erp' C-m

	#Start up sublime
	tmux send-keys -t development 'sublime &' C-m

	#Split the window
	tmux split-window -v -t development

	#Select a dev layout
	tmux select-layout -t development main-horizontal

	#Use the base index as 1
	tmux send-keys -t development:1.2 'cd ~/erp' C-m

	tmux new-window -n console -t development
	tmux send-keys -t development:2 'cd ~/erp' C-m

	tmux select-window -t development:1
	tmux attach -t development
fi
tmux attach -t development
