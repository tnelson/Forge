#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAR_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BASE_IMAGE="${BASE_IMAGE:-ubuntu:20.04@sha256:8feb4d8ca5354def3d8fce243717141ce31e2c428701f6682bd2fafe15388214}"
GLIBC_MAX="${GLIBC_MAX:-2.31}"

die() { echo "error: $*" >&2; exit 1; }

usage() {
  cat <<'EOF'
Usage: test-linux.sh [--arch x86_64|aarch64|all] [--dry-run] [--shell]

Tests the libraries in linux-<arch>/ (next to this script) inside an Ubuntu 20.04 container of
that CPU: portability checks (check-native-lib.py), then a real solve for each of MiniSat,
MiniSatProver, Glucose and Lingeling, including a non-empty unsat core for MiniSatProver.
EOF
}

ARCHES=""
DRY_RUN=0
SHELL_MODE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --arch)
      [ $# -ge 2 ] || die "--arch needs a value"
      case "$2" in
        x86_64|amd64) ARCHES="$ARCHES x86_64" ;;
        aarch64|arm64) ARCHES="$ARCHES aarch64" ;;
        all) ARCHES="$ARCHES x86_64 aarch64" ;;
        *) die "unknown architecture '$2'" ;;
      esac
      shift 2 ;;
    --dry-run) DRY_RUN=1; shift ;;
    --shell) SHELL_MODE=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown argument: $1" ;;
  esac
done

if [ -z "$ARCHES" ]; then
  case "$(uname -m)" in
    x86_64|amd64) ARCHES="x86_64" ;;
    aarch64|arm64) ARCHES="aarch64" ;;
    *) die "cannot infer an architecture; pass --arch" ;;
  esac
fi
ARCHES="$(echo $ARCHES | tr ' ' '\n' | awk '!seen[$0]++' | tr '\n' ' ')"
ARCHES="${ARCHES% }"

if [ "$DRY_RUN" = 0 ]; then
  command -v docker >/dev/null 2>&1 || die "docker is not installed or not on PATH"
  docker info >/dev/null 2>&1 || die "the docker daemon is not reachable (is Docker running?)"
fi

sha256_stdin() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi
}

TESTER_DOCKERFILE="ARG BASE_IMAGE=$BASE_IMAGE
FROM \${BASE_IMAGE}
ENV DEBIAN_FRONTEND=noninteractive TZ=UTC
RUN apt-get update -o Acquire::Retries=5 \\
 && apt-get install -y --no-install-recommends -o Acquire::Retries=5 python3 openjdk-17-jdk-headless \\
 && rm -rf /var/lib/apt/lists/*
"

tester_tag() {
  local h
  h="$( (printf '%s' "$TESTER_DOCKERFILE"; echo "$BASE_IMAGE") | sha256_stdin | cut -c1-12 )"
  echo "forge-native-tester:$h-$1"
}

JAVA_TEST='
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.satlab.SATSolver;

public class LoadTest {
    static int failures = 0;

    static void fail(String name, String why) {
        System.out.println(String.format("  %-14s FAIL: %s", name, why));
        failures++;
    }

    static void check(String name, SATFactory factory) {
        if (!SATFactory.available(factory)) {
            fail(name, "not available (the native library did not load or failed a trivial solve)");
            return;
        }

        // Satisfiable: clauses (1 or 2), (not 1 or 2), (1 or not 2) force both variables true.
        SATSolver sat = factory.instance();
        try {
            sat.addVariables(2);
            sat.addClause(new int[] {1, 2});
            sat.addClause(new int[] {-1, 2});
            sat.addClause(new int[] {1, -2});
            if (!sat.solve()) {
                fail(name, "a satisfiable formula was reported unsatisfiable");
                return;
            }
            if (!sat.valueOf(1) || !sat.valueOf(2)) {
                fail(name, "wrong model for a satisfiable formula");
                return;
            }
        } finally {
            sat.free();
        }

        // Unsatisfiable: add (not 1 or not 2) as well.
        SATSolver unsat = factory.instance();
        try {
            unsat.addVariables(2);
            unsat.addClause(new int[] {1, 2});
            unsat.addClause(new int[] {-1, 2});
            unsat.addClause(new int[] {1, -2});
            unsat.addClause(new int[] {-1, -2});
            if (unsat.solve()) {
                fail(name, "an unsatisfiable formula was reported satisfiable");
                return;
            }
            if (factory.prover()) {
                if (unsat instanceof SATProver prover) {
                    if (prover.proof().core().size() == 0) {
                        fail(name, "the proof of unsatisfiability has an empty core");
                        return;
                    }
                } else {
                    fail(name, "a prover factory did not produce a SATProver");
                    return;
                }
            }
        } finally {
            unsat.free();
        }

        System.out.println(String.format("  %-14s ok (loads; sat model correct; unsat detected%s)",
                name, factory.prover() ? "; unsat core non-empty" : ""));
    }

    public static void main(String[] args) {
        check("MiniSat", SATFactory.MiniSat);
        check("MiniSatProver", SATFactory.MiniSatProver);
        check("Glucose", SATFactory.Glucose);
        check("Lingeling", SATFactory.Lingeling);
        System.out.println(failures == 0 ? "all solvers passed" : failures + " solver(s) failed");
        System.exit(failures == 0 ? 0 : 1);
    }
}
'

CONTAINER_SCRIPT='
set -euo pipefail
[ "$(uname -m)" = "$ARCH" ] || { echo "container CPU is $(uname -m), expected $ARCH"; exit 1; }

command -v javac >/dev/null 2>&1 && command -v python3 >/dev/null 2>&1 \
  || { echo "this container lacks javac or python3; it must be the tester image built by test-linux.sh"; exit 1; }

echo "=== platform: $(uname -m), $(java -version 2>&1 | head -n1) ==="
echo ""
echo "=== verifier provenance (informational) ==="
have=$(sha256sum /native/check-native-lib.py | cut -d" " -f1)
manifest=/native/linux-$ARCH/MANIFEST.txt
if [ -f "$manifest" ]; then
  want=$(grep "^verifier sha256:" "$manifest" | tr -s " " | cut -d" " -f3)
  if [ -z "$want" ]; then
    echo "  note: MANIFEST.txt records no checker hash"
  elif [ "$want" = "$have" ]; then
    echo "  the build was checked with this same checker ($have)"
  else
    echo "  note: the build was checked with a different checker version ($want); this one is $have"
  fi
else
  echo "  note: no MANIFEST.txt in linux-$ARCH/"
fi

echo ""
echo "=== static checks (check-native-lib.py, glibc <= $GLIBC_MAX) ==="
python3 /native/check-native-lib.py --arch "$ARCH" --max-glibc "$GLIBC_MAX" /native/linux-$ARCH/*.so

echo ""
echo "=== JVM solve tests (kodkod SATFactory) ==="
mkdir -p /tmp/loadtest && cd /tmp/loadtest
printf "%s" "$JAVA_TEST" > LoadTest.java
javac -cp "/jars/*" LoadTest.java
java -Djava.library.path=/native/linux-$ARCH -cp "/jars/*:." LoadTest

echo ""
echo "=== ALL CHECKS PASSED ==="
'

test_one() {
  local arch="$1" platform libs
  case "$arch" in
    x86_64) platform="linux/amd64" ;;
    aarch64) platform="linux/arm64" ;;
  esac
  libs="$SCRIPT_DIR/linux-$arch"
  [ "$DRY_RUN" = 1 ] || [ -d "$libs" ] || die "no $libs directory (install libraries built for this CPU there first)"

  local tag cmd build_cmd
  tag="$(tester_tag "$arch")"
  build_cmd=(docker build --platform "$platform" --build-arg "BASE_IMAGE=$BASE_IMAGE" -t "$tag" -)
  if [ "$SHELL_MODE" = 1 ]; then
    cmd=(docker run --rm -it --platform "$platform"
         -v "$SCRIPT_DIR:/native:ro" -v "$JAR_DIR:/jars:ro" -w /tmp
         -e "ARCH=$arch" -e "GLIBC_MAX=$GLIBC_MAX" "$tag" bash)
  else
    cmd=(docker run --rm --platform "$platform"
         -v "$SCRIPT_DIR:/native:ro" -v "$JAR_DIR:/jars:ro" -w /tmp
         -e "ARCH=$arch" -e "GLIBC_MAX=$GLIBC_MAX" -e "JAVA_TEST=$JAVA_TEST"
         "$tag" bash -c "$CONTAINER_SCRIPT")
  fi

  echo "== testing linux-$arch ($platform, tester image $tag, base $BASE_IMAGE)"
  if [ "$DRY_RUN" = 1 ]; then
    echo "   (build the tester image if it does not exist, from the Dockerfile text in this script)"
    printf '   '; printf '%q ' "${build_cmd[@]}"; echo "< Dockerfile"
    printf '   '; printf '%q ' "${cmd[0]}" "${cmd[1]}" "${cmd[2]}" "${cmd[3]}" "${cmd[4]}"; echo "... (container script elided)"
    return 0
  fi

  if ! docker image inspect "$tag" >/dev/null 2>&1; then
    echo "== building tester image $tag (once; later runs reuse it)"
    printf '%s' "$TESTER_DOCKERFILE" | "${build_cmd[@]}"
  fi
  "${cmd[@]}"
}

for arch in $ARCHES; do
  test_one "$arch"
done
