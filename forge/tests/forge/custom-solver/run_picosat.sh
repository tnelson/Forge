#!/bin/sh

# Wrap picosat as mock custom solver for instance-set comparison in custom_solver_next.rkt.
# For CI only; CI installs picosat.
command -v picosat >/dev/null 2>&1 || { echo "picosat is not installed" >&2; exit 2; }
exec picosat "$1"
