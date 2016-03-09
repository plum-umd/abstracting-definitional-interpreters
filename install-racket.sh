# IMPORTANT: Your .travis.yml must pipe this to bash (not to sh)!
# In the Travis CI environment a #!/bin/bash shebang here won't help.

set -e

if [[ "$RACKET_VERSION" = "HEAD" ]]; then
    URL="http://plt.eecs.northwestern.edu/snapshots/current/installers/racket-test-current-x86_64-linux-precise.sh"
elif [[ "$RACKET_VERSION" = "SCOPE_SNAPSHOT" ]]; then
    URL="http://www.cs.utah.edu/~mflatt/tmp/scope-snapshot/installers/racket-current-x86_64-linux.sh"
elif [[ "$RACKET_VERSION" = "RELEASE" ]]; then
    URL="http://pre-release.racket-lang.org/installers/racket-current-x86_64-linux-ubuntu-precise.sh"
elif [[ "$RACKET_VERSION" = 5.9* ]]; then
    URL="http://download.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-ubuntu-quantal.sh"
elif [[ "$RACKET_VERSION" = 6.* ]]; then
    URL="http://download.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-ubuntu-precise.sh"
else
    URL="http://download.racket-lang.org/installers/${RACKET_VERSION}/racket/racket-${RACKET_VERSION}-bin-x86_64-linux-debian-squeeze.sh"
fi

# Older .travis.yml files don't set $RACKET_DIR (the Racket install
# directory) explicitly and expect it to be /usr/racket.
if [[ "$RACKET_DIR" = "" ]]; then
    RACKET_DIR=/usr/racket
fi

INSTALLER="./racket-${RACKET_VERSION}.sh"

echo "Downloading $URL to $INSTALLER:"
curl --insecure -L -o $INSTALLER $URL

echo "Making $INSTALLER executable:"
chmod u+rx "$INSTALLER"

# Only use sudo if installing to /usr
if [[ "$RACKET_DIR" = /usr* ]]; then
    RUN_INSTALLER="sudo ${INSTALLER}"
else
    RUN_INSTALLER="${INSTALLER}"
fi

echo "Running $RUN_INSTALLER to install Racket:"

$RUN_INSTALLER <<EOF
no
"$RACKET_DIR"

EOF

exit 0
