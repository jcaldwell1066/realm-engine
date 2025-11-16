#!/bin/bash
# term-box.sh - Enhanced terminal-aware wrapper for /usr/bin/boxes
# Auto-detects terminal size and provides fancy formatting options
#
# Usage:
#   echo "text" | term-box.sh
#   echo "text" | term-box.sh --fill
#   echo "text" | term-box.sh --style stone --center
#   echo "text" | term-box.sh --fill --title "My Title"

# Get terminal size (or pane size if in tmux)
if [ -n "$TMUX_PANE" ]; then
    TERM_WIDTH=$(tmux display -p -t "$TMUX_PANE" '#{pane_width}')
    TERM_HEIGHT=$(tmux display -p -t "$TMUX_PANE" '#{pane_height}')
else
    TERM_WIDTH=$(tput cols 2>/dev/null || echo 80)
    TERM_HEIGHT=$(tput lines 2>/dev/null || echo 24)
fi

# Defaults
STYLE="stone"
ALIGN="left"
FILL=false
TITLE=""
WIDTH=$((TERM_WIDTH - 4))
PADDING=2

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --style|-s)
            STYLE="$2"
            shift 2
            ;;
        --center|-c)
            ALIGN="center"
            shift
            ;;
        --left|-l)
            ALIGN="left"
            shift
            ;;
        --right|-r)
            ALIGN="right"
            shift
            ;;
        --fill|-f)
            FILL=true
            shift
            ;;
        --title|-t)
            TITLE="$2"
            shift 2
            ;;
        --width|-w)
            WIDTH="$2"
            shift 2
            ;;
        --padding|-p)
            PADDING="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

# Read input
INPUT=$(cat)

# If fill mode, expand with padding
if [ "$FILL" = true ]; then
    WIDTH=$((TERM_WIDTH - PADDING * 2))

    # Add vertical padding by adding empty lines
    IFS=$'\n' read -rd '' -a LINES <<< "$INPUT"
    LINE_COUNT=${#LINES[@]}

    # Calculate vertical padding
    CONTENT_HEIGHT=$((LINE_COUNT + 4))  # +4 for borders and spacing
    VERT_PADDING=$(( (TERM_HEIGHT - CONTENT_HEIGHT) / 2 ))

    # Add top padding
    if [ $VERT_PADDING -gt 0 ]; then
        for ((i=0; i<VERT_PADDING; i++)); do
            echo ""
        done
    fi
fi

# Add title if specified
if [ -n "$TITLE" ]; then
    # Create title box
    echo "$TITLE" | /usr/bin/boxes -d "$STYLE" -s "$WIDTH" -a c
    echo ""
    # Process content
    echo "$INPUT" | /usr/bin/boxes -d "$STYLE" -s "$WIDTH" -a "${ALIGN:0:1}"
else
    # Just process input
    case "$ALIGN" in
        center|centre)
            echo "$INPUT" | /usr/bin/boxes -d "$STYLE" -s "$WIDTH" -a c
            ;;
        right)
            echo "$INPUT" | /usr/bin/boxes -d "$STYLE" -s "$WIDTH" -a r
            ;;
        *)
            echo "$INPUT" | /usr/bin/boxes -d "$STYLE" -s "$WIDTH" -a l
            ;;
    esac
fi

# Add bottom padding if fill mode
if [ "$FILL" = true ] && [ $VERT_PADDING -gt 0 ]; then
    for ((i=0; i<VERT_PADDING; i++)); do
        echo ""
    done
fi
