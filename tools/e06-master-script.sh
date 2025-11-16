#!/bin/bash
# e06-master-script.sh - Complete E06 orchestration
# Single script controls everything - no manual intervention

set -e

PROJECT_ROOT="/home/be-dev-agent/projects/experiments/skunkworks/exploration/adventure-engine"
cd "$PROJECT_ROOT"

TOP="foo:1.2"
BOTTOM="foo:1.3"

# Clear everything
tmux clear-history -t foo:1.1
tmux clear-history -t foo:1.2
tmux clear-history -t foo:1.3

tmux send-keys -t "$TOP" C-c 2>/dev/null || true
tmux send-keys -t "$BOTTOM" "\$quit" Enter 2>/dev/null || true
sleep 1

tmux send-keys -t "$TOP" "clear" Enter
tmux send-keys -t "$BOTTOM" "clear" Enter

# Helper: Show card in top panel
show_card() {
    local card="$1"
    local duration="$2"
    tmux send-keys -t "$TOP" "clear && cat e06-cards/$card | ./tools/term-box.sh --fill --center --style stone" Enter
    sleep "$duration"
}

# Helper: Type command in bottom panel
type_cmd() {
    local cmd="$1"
    local wait="$2"
    ./tools/type-to-pane.sh "$BOTTOM" "$cmd"
    sleep "$wait"
}

# START RECORDING
echo "🎬 E06 Recording Starting..."

# Card 1: Title
show_card "01-title.txt" 3

# Card 2: What is DSL
show_card "02-what-is-dsl.txt" 4

# Card 3: The Stack
show_card "03-the-stack.txt" 4

# Start game (silent)
tmux send-keys -t "$BOTTOM" "stack exec adventure-cli data/example-adventure/world.txt" Enter
sleep 3

# Card 4: Ready to play
show_card "04-ready-to-play.txt" 3

# Card 5: Command - look
show_card "05-cmd-look.txt" 2
type_cmd "look" 2

# Card 6: Command - pickup
show_card "06-cmd-pickup.txt" 2
type_cmd "pickup Rusty Key" 2

# Card 7: Command - unlock
show_card "07-cmd-unlock.txt" 2
type_cmd "use Rusty Key on Front Door" 2

# Card 8: Command - walk
show_card "08-cmd-walk.txt" 2
type_cmd "walk Front Door" 2

# Card 9: Exploring
show_card "09-exploring.txt" 3
type_cmd "walk Kitchen Door" 2

# Card 10: Command - dig
show_card "10-cmd-dig.txt" 2
type_cmd "dig" 3
type_cmd "pickup Paper Note" 2

# Card 11: The puzzle
show_card "11-the-puzzle.txt" 3
type_cmd "walk Kitchen Door Return" 1
type_cmd "walk Library Door" 2

# Card 12: Final unlock
show_card "12-final-unlock.txt" 2
type_cmd "use Paper Note on Treasure Chest" 2

# Card 13: Victory
show_card "13-victory.txt" 2
type_cmd "pickup Gold Coin" 2
type_cmd "inventory" 3

# Card 14: Wrap
show_card "14-wrap.txt" 4

echo ""
echo "🎬 E06 Recording Complete!"
echo ""
echo "Total runtime: ~60 seconds"
echo "Commands: 12"
echo "Cards: 14"
