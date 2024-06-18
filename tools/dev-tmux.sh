#!/bin/sh
cd ..
tmux new-session -d 'cargo watch -x build'
tmux split-window -v 'cd lsp && cargo watch -x build'
tmux split-window -h 'cd vscode && npm run watch'
tmux -2 attach-session -d
