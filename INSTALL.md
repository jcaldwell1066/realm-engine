# Installation Guide

## Debian/Ubuntu Setup

### Required System Packages

```bash
# PostgreSQL (for world persistence)
sudo apt install postgresql postgresql-client libpq-dev

# Build tools
sudo apt install build-essential curl libgmp-dev zlib1g-dev

# Optional: Demo tools
sudo apt install boxes tmux
```

### Install Stack (Haskell build tool)

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Or via apt (may be older):
```bash
sudo apt install haskell-stack
```

### Build the Project

```bash
# Clone repository
git clone https://github.com/jcaldwell1066/realm-engine.git
cd realm-engine

# Build (first time takes ~15-20 minutes)
stack build

# Run tests
stack test

# Install executables
stack install
```

### Run the Demo

```bash
# Play the example world
stack exec adventure-cli data/example-adventure/world.txt

# Run Episode 6 orchestrated demo
./tools/e06-master-script.sh
```

## Troubleshooting

### Missing libpq

```bash
sudo apt install libpq-dev
stack clean
stack build
```

### Stack version issues

```bash
stack upgrade
stack setup
```

### GHC compilation errors

```bash
stack clean --full
rm -rf .stack-work
stack build
```

## Docker Alternative

See `docker/README.md` for containerized setup (if available).

## Dependencies

Stack will automatically install:
- GHC (Haskell compiler)
- Required Haskell libraries (see package.yaml)

Main dependencies:
- megaparsec (parsing)
- aeson (JSON)
- postgresql-simple (database)
- brick (TUI - if used)
- containers, text, bytestring (stdlib)

All managed by stack.yaml resolver.
