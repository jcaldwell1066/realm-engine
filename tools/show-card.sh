#!/bin/bash
# show-card.sh - Display a card in tmux pane using boxes
# Usage: show-card.sh <pane> <card-file> [duration]

PANE="$1"
CARD_FILE="$2"
DURATION="${3:-3}"

if [ ! -f "$CARD_FILE" ]; then
    echo "Card file not found: $CARD_FILE" >&2
    exit 1
fi

# Send to pane
tmux send-keys -t "$PANE" "clear && cat '$CARD_FILE' | /home/be-dev-agent/projects/experiments/skunkworks/exploration/adventure-engine/tools/term-box.sh --fill --center --style stone" Enter

# Optional: sleep for duration (if >0)
if [ "$DURATION" != "0" ]; then
    sleep "$DURATION"
fi
