#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAR_DIR="$(cd "$HERE/.." && pwd)"

die() { echo "error: $*" >&2; exit 1; }

usage() {
  cat <<'EOF'
Usage: test-native.sh --dir NAME

Loads the native solver libraries in native/NAME with the bundled kodkod.jar and a real JVM,
then solves a satisfiable and an unsatisfiable formula with each solver and gets an unsat
core from MiniSatProver (NativeSolverTest.java). Runs on Linux, macOS, and Git Bash on Windows.

  --dir NAME   one of the directories next to this script, for example linux-x86_64,
               linux-aarch64, macos, windows-x86_64. A value containing a slash is used as a
               path instead, to test libraries before installing them.

Needs a JDK (javac and java on PATH, or JAVA_HOME).
EOF
}

NAME=""
while [ $# -gt 0 ]; do
  case "$1" in
    --dir) [ $# -ge 2 ] || die "--dir needs a value"; NAME="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown argument: $1 (try --help)" ;;
  esac
done

[ -n "$NAME" ] || die "--dir is required (try --help)"
case "$NAME" in
  */*) LIBS="$NAME" ;;
  *) LIBS="$HERE/$NAME" ;;
esac
[ -d "$LIBS" ] || die "no such native directory: $LIBS"
LIBS="$(cd "$LIBS" && pwd)"

JAVA_BIN="java"
JAVAC_BIN="javac"
if [ -n "${JAVA_HOME:-}" ]; then
  JAVA_BIN="$JAVA_HOME/bin/java"
  JAVAC_BIN="$JAVA_HOME/bin/javac"
fi
command -v "$JAVAC_BIN" >/dev/null 2>&1 || [ -x "$JAVAC_BIN" ] || die "no javac found; a full JDK is required"

SEP=":"
to_java_path() { printf '%s' "$1"; }
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    SEP=";"
    to_java_path() { cygpath -m "$1"; }
    ;;
esac

CP=""
for jar in "$JAR_DIR"/*.jar; do
  CP="${CP:+$CP$SEP}$(to_java_path "$jar")"
done

CLASS_DIR="$(mktemp -d "${TMPDIR:-/tmp}/forge-native-test.XXXXXX")"
trap 'rm -rf "$CLASS_DIR"' EXIT

echo "=== native libraries in $NAME ==="
ls "$LIBS"
echo ""
echo "=== JVM solve tests ($NAME) ==="
"$JAVAC_BIN" -cp "$CP" -d "$(to_java_path "$CLASS_DIR")" "$(to_java_path "$HERE/NativeSolverTest.java")"
"$JAVA_BIN" -Djava.library.path="$(to_java_path "$LIBS")" -cp "$CP$SEP$(to_java_path "$CLASS_DIR")" NativeSolverTest
