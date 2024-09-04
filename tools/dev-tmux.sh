#!/bin/sh
cd ..
tmux new-session -d 'cargo watch -x build'
tmux split-window -v 'cd lsp && cargo watch -x build'
tmux split-window -h 'cd tools/vscode && npm run watch; sleep 10n'
tmux -2 attach-session -d
