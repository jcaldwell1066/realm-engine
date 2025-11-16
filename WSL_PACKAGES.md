# WSL Ubuntu 24.04 Package Inventory

**System Information:**
- OS: Ubuntu 24.04.3 LTS (Noble Numbat)
- Kernel: Linux 4.4.0
- Total Packages Installed: 670

This document lists packages available on the WSL Ubuntu system that can be used for container configuration.

## Essential Build Tools

### Compilers and Build Systems
- **gcc** (13) - GNU C Compiler
- **g++** (13) - GNU C++ Compiler
- **clang** (18) - LLVM C/C++ Compiler
- **build-essential** - Meta-package for build tools
- **make** - GNU Make
- **cmake** - Cross-platform build system
- **ninja-build** - Small build system
- **autoconf** - Automatic configure script builder
- **automake** - Tool for generating Makefiles
- **libtool** - Generic library support script
- **m4** - Macro processing language
- **bison** - Parser generator
- **patch** - Apply diff files

### Debugging and Analysis
- **gdb** - GNU Debugger
- **valgrind** - Memory debugging and profiling
- **llvm-18** - LLVM compiler infrastructure
- **clang-tidy** (18) - Linter for C/C++
- **clang-format** (18) - Code formatter

## Haskell Project Requirements

### Core Dependencies (Required for Stack/GHC)
- **libgmp10** - GNU Multi-Precision arithmetic library
- **libffi8** - Foreign Function Interface library
- **libffi-dev** - FFI development files
- **zlib1g** - Compression library
- **zlib1g-dev** - Zlib development files
- **libncurses6** - Terminal handling (for GHCi)
- **libncursesw6** - Wide-character ncurses
- **libncurses-dev** - Ncurses development files
- **libtinfo6** - Terminal info library

### Graphics and GUI Libraries (for monomer, brick, vty)
- **libgl1** - OpenGL library
- **libgl1-mesa-dri** - Mesa DRI drivers
- **libglx0** - GLX library
- **libglvnd0** - GL vendor neutral dispatch
- **libx11-6** - X11 client library
- **libx11-dev** - X11 development files
- **libxext6** - X11 extensions library
- **libxext-dev** - X11 extensions development
- **libxrender1** - X Rendering Extension
- **libxrender-dev** - XRender development files
- **libxft2** - X FreeType library (for fonts)
- **libfreetype6** - FreeType font rendering
- **libfreetype-dev** - FreeType development
- **libfontconfig1** - Font configuration library
- **libfontconfig-dev** - Fontconfig development
- **libpango-1.0-0** - Text layout and rendering
- **libpangocairo-1.0-0** - Pango Cairo backend
- **libcairo2** - 2D graphics library
- **libcairo2-dev** - Cairo development files
- **libpixman-1-0** - Pixel manipulation library
- **libpixman-1-dev** - Pixman development
- **mesa-libgallium** - Mesa Gallium drivers

### Image Processing (for JuicyPixels)
- **libpng16-16t64** - PNG library
- **libpng-dev** - PNG development files
- **libjpeg-turbo8** - JPEG library
- **libwebp7** - WebP image format
- **libimagequant0** - Image quantization

### Database (for postgresql-simple)
- **postgresql-16** - PostgreSQL database server
- **postgresql-client-16** - PostgreSQL client
- **libpq5** - PostgreSQL C client library
- **libpq-dev** - PostgreSQL development files
- **postgresql-common** - PostgreSQL common files
- **postgresql-client-common** - Client common files

### Text and String Processing
- **libpcre3** - Perl Compatible Regular Expressions
- **libpcre2-8-0** - PCRE2 library
- **libpcre2-dev** - PCRE2 development
- **libiconv-hook-dev** - Character encoding conversion

### Compression and Archive
- **libbz2-1.0** - bzip2 library
- **libbz2-dev** - bzip2 development
- **liblzma5** - XZ compression library
- **liblzma-dev** - XZ development files
- **libzip4** - ZIP archive library
- **zlib1g** - gzip compression

### System Libraries
- **libc6-dev** - GNU C Library development
- **libstdc++-13-dev** - C++ standard library development
- **libssl3** - SSL/TLS library
- **libssl-dev** - OpenSSL development
- **libreadline8** - GNU readline library
- **libreadline-dev** - Readline development
- **libyaml-0-2** - YAML parser library
- **libyaml-dev** - YAML development

## Container Base Package Recommendations

### Minimal Dockerfile Base Packages
For a minimal Haskell Stack container, install:

```dockerfile
# Base system
RUN apt-get update && apt-get install -y \
    # Build essentials
    build-essential \
    curl \
    wget \
    git \
    # Core Haskell dependencies
    libgmp-dev \
    libffi-dev \
    libncurses-dev \
    libtinfo-dev \
    zlib1g-dev \
    # Graphics libraries (for monomer/brick/vty)
    libgl1-mesa-dev \
    libx11-dev \
    libxext-dev \
    libxrender-dev \
    libxrandr-dev \
    libxinerama-dev \
    libxcursor-dev \
    libxi-dev \
    libxft-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libpango1.0-dev \
    # Database support
    postgresql-client \
    libpq-dev \
    # Image processing
    libpng-dev \
    libjpeg-dev \
    # Compression
    libbz2-dev \
    liblzma-dev \
    # SSL/TLS
    libssl-dev \
    # Cleanup
    && rm -rf /var/lib/apt/lists/*
```

### Extended Dockerfile (with development tools)
For a development container with additional tools:

```dockerfile
RUN apt-get update && apt-get install -y \
    # All minimal packages above, plus:
    # Debugging and analysis
    gdb \
    valgrind \
    # Version control
    git \
    # Text editors
    vim \
    nano \
    # Process management
    tmux \
    screen \
    # Network tools
    netcat-openbsd \
    curl \
    wget \
    # System utilities
    lsof \
    strace \
    && rm -rf /var/lib/apt/lists/*
```

## Stack Installation

Stack is not available via apt packages. Install via curl:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Or download specific version:

```bash
wget -qO- https://github.com/commercialhaskell/stack/releases/download/v2.15.1/stack-2.15.1-linux-x86_64.tar.gz | tar xz --strip-components=1 -C /usr/local/bin
```

## Package Categories Summary

| Category | Count | Key Packages |
|----------|-------|--------------|
| Build Tools | 15+ | gcc, g++, make, cmake, clang |
| Haskell Core | 10+ | libgmp, libffi, ncurses, zlib |
| Graphics/GUI | 30+ | X11, Mesa, Cairo, Pango |
| Database | 6 | PostgreSQL 16, libpq |
| Image Processing | 5+ | libpng, libjpeg, libwebp |
| Compression | 6+ | zlib, bzip2, lzma |
| Development | 50+ | Various -dev packages |

## Notes for Container Configuration

1. **WSL vs Container**: The WSL environment is Ubuntu 24.04, not Debian 13. Use `ubuntu:24.04` as base image for maximum compatibility.

2. **No Haskell Pre-installed**: No GHC or Stack packages are currently installed. These need to be added.

3. **Graphics Stack Complete**: Full X11, Mesa, and GUI libraries are available for applications using monomer/brick/vty.

4. **PostgreSQL 16**: Latest stable PostgreSQL is installed and available.

5. **Development Headers**: Most `-dev` packages are installed, providing headers for compilation.

6. **Multi-architecture**: System is x86_64 (amd64). Ensure container targets same architecture.

## Full Package List

Complete list of all 670 installed packages is available at: `/tmp/all-packages.txt`

To view:
```bash
cat /tmp/all-packages.txt
```

To search for specific packages:
```bash
grep -i "search-term" /tmp/all-packages.txt
```
