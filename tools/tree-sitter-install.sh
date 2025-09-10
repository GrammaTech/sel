#!/usr/bin/env bash
set -eux

WORKDIR=${WORKDIR:-$(mktemp -d)}
PREFIX=${PREFIX:-/usr}
CC=${CC:-cc}
CXX=${CXX:-c++}

if [ "$(uname)" == "Darwin" ];then
    EXT=dylib;
else
    EXT=so
fi

# Withheld languages: agda c-sharp julia ocaml/interface ocaml/ocaml php ql ruby scala
declare -ar default_languages=(
    ada bash c commonlisp cpp css go html java javascript jsdoc json python regex rust
    typescript/tsx typescript/typescript
    yaml
)

NOPIN=${NOPIN:-}

# Pinned versions.
declare -A pins
pins[tree-sitter]=efe009f4

pins[ada]=e8e2515
pins[bash]=275effd
pins[c]=f357890
pins[commonlisp]=c7e8149
pins[cpp]=2f3d62d
pins[css]=a03f1d2
pins[go]=aeb2f33
pins[html]=29f53d8
pins[java]=ac14b4b
pins[javascript]=7858313
pins[jsdoc]=189a6a4
pins[json]=368736a
# Special case: pin tree-sitter-python to 24b530c until https://github.com/tree-sitter/tree-sitter/issues/1654 is resolved
pins[python]=24b530c
pins[regex]=e1cfca3
pins[rust]=fbf9e50
pins[typescript]=1b3ba31
pins[yaml]=0e36bed

pins[typescript/typescript]=${pins[typescript]}
pins[typescript/tsx]=${pins[typescript]}

# Declared repositories.
declare -A repos
repos[ada]=https://github.com/briot/tree-sitter-ada.git
repos[commonlisp]=https://github.com/theHamsta/tree-sitter-commonlisp.git
repos[cpp]=https://github.com/ruricolist/tree-sitter-cpp.git
repos[yaml]=https://github.com/ikatyang/tree-sitter-yaml.git

cd "$WORKDIR"

pin_repo() {
    if [ -z "$NOPIN" ]; then
        readonly rev=$1
        if [ -n "$rev" ]; then
            # Pull only if the commit does not exist locally.
            git cat-file -t "$rev" &>/dev/null || git pull
            git reset --hard "$rev"
        fi
    else
        git pull
    fi
}

# Note we don't shallow clone (we couldn't pin the repositories that
# way) but we do clone only the default branch.

# Install tree-sitter itself.
if [ ! -d tree-sitter ]; then
   git clone --single-branch https://github.com/tree-sitter/tree-sitter
fi
(
    cd tree-sitter
    pin_repo ${pins[tree-sitter]}
    CFLAGS=-O3 PREFIX=${PREFIX} make all install
)

# Install languages.

declare -a languages
if [ -z "${1:-}" ]; then
    languages=( "${default_languages[@]}" )
else
    languages=( "$@" )
fi

for language in "${languages[@]}"; do
    [ -d "tree-sitter-${language%/*}" ] || git clone --single-branch ${repos[$language]:-https://github.com/tree-sitter/tree-sitter-${language%/*}};
    # Use a subshell to avoid directory juggling.
    (
        cd "tree-sitter-${language}/src";
        pin_repo ${pins[$language]};
        # NB Pass -I. when compiling to use the local tree_sitter/parser.h.
        if test -f "scanner.cc"; then
            ${CXX} -I. -fPIC scanner.cc -c -lstdc++;
            ${CC} -I. -std=c99 -fPIC parser.c -c;
            ${CXX} -shared scanner.o parser.o -o "${PREFIX}/lib/tree-sitter-${language//\//-}.${EXT}";
        elif test -f "scanner.c"; then
            ${CC} -I. -std=c99 -fPIC scanner.c -c;
            ${CC} -I. -std=c99 -fPIC parser.c -c;
            ${CC} -shared scanner.o parser.o -o "${PREFIX}/lib/tree-sitter-${language//\//-}.${EXT}";
        else
            ${CC} -I. -std=c99 -fPIC parser.c -c;
            ${CC} -shared parser.o -o "${PREFIX}/lib/tree-sitter-${language//\//-}.${EXT}";
        fi;
        mkdir -p "${PREFIX}/share/tree-sitter/${language}/";
        cp grammar.json node-types.json "${PREFIX}/share/tree-sitter/${language}";
    )
done
