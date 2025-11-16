# The Trust Bridge

A team building realm for the Realm Engine focused on **communication** and **trust**.

## Overview

The Trust Bridge is a collaborative puzzle experience where teams must share information discovered in separate chambers to unlock a bridge together. It demonstrates that some challenges cannot be solved alone and require combining different perspectives and discoveries.

## Team Building Focus

- **Communication**: Teams must verbally share what they find in separate chambers
- **Trust**: Players must rely on teammates' accurate reporting
- **Collaboration**: Success requires combining information from multiple sources
- **Active Listening**: Teams must carefully process shared information

## How to Facilitate

1. **Divide the team** into two groups (or have them explore separately)
2. **Assign exploration**: Group A explores East Chamber, Group B explores West Chamber
3. **Communication challenge**: Groups must communicate their findings verbally (without showing screens if playing remotely)
4. **Collaborate**: Together, solve the bridge mechanism puzzle
5. **Debrief**: Discuss how communication and trust enabled success

## The Puzzle

- The **East Chamber** contains a Stone Tablet with the inscription: *"TRUST begins the journey"*
- The **West Chamber** contains a Brass Plaque with the inscription: *"UNITY completes the path"*
- The **Bridge Mechanism** requires both artifacts to stabilize the bridge
- Only by **sharing information** can teams discover they need both items

## Learning Objectives

After completing this realm, participants will have practiced:

- Clear communication of discovered information
- Exploring different paths and sharing findings
- Understanding that some challenges require multiple perspectives
- Building trust through collaborative problem-solving
- Combining individual discoveries into team solutions

## Debrief Questions

Use these questions after the experience to facilitate reflection:

1. How did you decide to split up the exploration?
2. What communication strategies worked well?
3. How did you ensure accurate information sharing?
4. What would have happened if one person didn't share their discovery?
5. How does this mirror real-world team challenges?

## Technical Details

- **Difficulty**: Beginner
- **Duration**: ~15 minutes
- **Rooms**: 5
- **Objects**: 4
- **Category**: Team Building
- **Recommended Team Size**: 2-4 people

## Quick Walkthrough

1. Read the Ancient Journal in the Entrance Hall for context
2. Split exploration: one person/group goes East, another goes West
3. **East Chamber**: Pick up the Stone Tablet and note its inscription
4. **West Chamber**: Pick up the Brass Plaque and note its inscription
5. **Share your findings** with your team (this is the key!)
6. Go to the Bridge Chamber with both items
7. Use both artifacts on the Bridge Mechanism
8. Cross the now-stable bridge to victory!

## Building the Realm

To compile this realm:

```bash
stack exec adventure-maker data/trust-bridge
```

This will generate:
- `world.json` - JSON representation
- `world.sql` - SQL schema
- `build/game.zip` - Packaged game file

## Playing the Realm

Load the realm using one of the Realm Engine clients:

```bash
# CLI version
stack exec adventure-cli data/trust-bridge

# TUI version
stack exec adventure-tui data/trust-bridge

# GUI version
stack exec adventure-engine-exe data/trust-bridge
```

## Author

Realm Engine Team - Created November 16, 2025

## License

Part of the Realm Engine project. See main LICENSE file.
