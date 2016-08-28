#!/usr/bin/env bash

# config
BINARY_NAME="groupme-stuff"
LIBS_DIR="lib"

# build with cabal
cabal build

# strip and pack
cd dist/build/groupme-stuff/
mkdir -p ${LIBS_DIR}
strip -s ${BINARY_NAME}
for f in $(ldd ${BINARY_NAME}  | fgrep '=>' | awk '{print $3}') ; do cp -L $f lib/ ; done
upx ${BINARY_NAME}

# generate shell script
cat > ${BINARY_NAME}.sh << EOF
#!/bin/sh
STANDALONE_DIR="\$(dirname \$(readlink -f \$0))"
LD_LIBS=\$(echo -n \$(find \${STANDALONE_DIR}/lib/ -type f) |tr ' ' ':')
LD_PRELOAD=\${LD_LIBS} \${STANDALONE_DIR}/groupme-stuff \$*
EOF
chmod +x ${BINARY_NAME}.sh

# bundle into tarball
tar -cpzf target.tar.gz ${BINARY_NAME} ${BINARY_NAME}.sh ${LIBS_DIR}