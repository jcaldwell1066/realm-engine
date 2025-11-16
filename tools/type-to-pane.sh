#!/bin/bash
# type-to-pane.sh - Send commands to tmux pane character-by-character
# Simulates human typing for demos and presentations
#
# Usage:
#   type-to-pane.sh <pane> "command to type"
#   type-to-pane.sh <pane> --script demo.txt
#   type-to-pane.sh <pane> --fast "quick command"
#   type-to-pane.sh <pane> --delay 0.1 "slow typing"

set -e

PANE="$1"
shift

# Defaults
CHAR_DELAY=0.08  # 80ms between characters (slower, more readable)
WORD_DELAY=0.20  # 200ms after spaces
ENTER_DELAY=0.8  # 800ms before hitting enter
AUTO_ENTER=true

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --fast|-f)
            CHAR_DELAY=0.02
            WORD_DELAY=0.05
            shift
            ;;
        --slow|-s)
            CHAR_DELAY=0.10
            WORD_DELAY=0.25
            shift
            ;;
        --delay|-d)
            CHAR_DELAY="$2"
            shift 2
            ;;
        --no-enter|-n)
            AUTO_ENTER=false
            shift
            ;;
        --script)
            SCRIPT_FILE="$2"
            shift 2
            ;;
        *)
            COMMAND="$1"
            shift
            ;;
    esac
done

# Function to type a single command
type_command() {
    local cmd="$1"
    local len=${#cmd}

    for ((i=0; i<len; i++)); do
        char="${cmd:$i:1}"

        # Send character
        tmux send-keys -t "$PANE" -l "$char"

        # Delay based on character type
        if [[ "$char" == " " ]]; then
            sleep "$WORD_DELAY"
        else
            sleep "$CHAR_DELAY"
        fi
    done

    # Send Enter if auto-enter enabled
    if [ "$AUTO_ENTER" = true ]; then
        sleep "$ENTER_DELAY"
        tmux send-keys -t "$PANE" Enter
    fi
}

# Main execution
if [ -n "$SCRIPT_FILE" ]; then
    # Read commands from script file
    if [ ! -f "$SCRIPT_FILE" ]; then
        echo "Error: Script file not found: $SCRIPT_FILE" >&2
        exit 1
    fi

    echo "Typing script: $SCRIPT_FILE to pane $PANE"

    while IFS= read -r line || [ -n "$line" ]; do
        # Skip comments and empty lines
        [[ "$line" =~ ^#.*$ ]] && continue
        [[ -z "$line" ]] && continue

        # Handle special directives
        if [[ "$line" =~ ^WAIT[[:space:]]+([0-9.]+) ]]; then
            wait_time="${BASH_REMATCH[1]}"
            echo "  [Waiting ${wait_time}s...]"
            sleep "$wait_time"
            continue
        fi

        if [[ "$line" =~ ^PAUSE ]]; then
            echo "  [Paused - press Enter to continue]"
            read -r
            continue
        fi

        # Type the command
        echo "  Typing: $line"
        type_command "$line"
        sleep 1  # Pause between commands
    done < "$SCRIPT_FILE"

    echo "Script complete!"

elif [ -n "$COMMAND" ]; then
    # Type single command
    echo "Typing to $PANE: $COMMAND"
    type_command "$COMMAND"
else
    echo "Usage: type-to-pane.sh <pane> [options] \"command\""
    echo "       type-to-pane.sh <pane> --script file.txt"
    echo ""
    echo "Options:"
    echo "  --fast, -f          Fast typing (0.02s delay)"
    echo "  --slow, -s          Slow typing (0.10s delay)"
    echo "  --delay, -d N       Custom delay (seconds)"
    echo "  --no-enter, -n      Don't press Enter after typing"
    echo "  --script FILE       Read commands from file"
    echo ""
    echo "Script file directives:"
    echo "  # comment           Comment line (ignored)"
    echo "  WAIT N              Wait N seconds"
    echo "  PAUSE               Wait for user to press Enter"
    echo ""
    echo "Examples:"
    echo "  type-to-pane.sh foo:1.3 \"look\""
    echo "  type-to-pane.sh foo:1.3 --fast \"take key\""
    echo "  type-to-pane.sh foo:1.3 --script game-demo.txt"
    exit 1
fi
