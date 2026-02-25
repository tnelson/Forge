#!/usr/bin/env bash
set -euo pipefail

DEFAULT_REPO="sidprasad/sterling-ts"
DEFAULT_TAG="latest"
DEFAULT_ASSET="sterling-forge.zip"



usage() {
  cat <<'USAGE'
Usage: ./update-sterling.sh [tag]
       ./update-sterling.sh --tag vX.Y.Z [--repo owner/name] [--asset sterling-forge.zip]
       ./update-sterling.sh --url https://github.com/owner/repo/releases/download/vX.Y.Z/sterling-forge.zip

Environment overrides:
  STERLING_REPO, STERLING_TAG, STERLING_ASSET, STERLING_URL
USAGE
}

get_latest_tag() {
  local repo_name="$1"
  local api_url="https://api.github.com/repos/${repo_name}/releases/latest"
  local response=""

  if command -v curl >/dev/null 2>&1; then
    response="$(curl -fsSL "$api_url")"
  elif command -v wget >/dev/null 2>&1; then
    response="$(wget -qO- "$api_url")"
  else
    echo "Missing required tool: curl or wget" >&2
    exit 1
  fi

  echo "$response" | sed -n 's/.*"tag_name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' | head -n 1
}

repo="${STERLING_REPO:-$DEFAULT_REPO}"
tag="${STERLING_TAG:-$DEFAULT_TAG}"
asset="${STERLING_ASSET:-$DEFAULT_ASSET}"
url="${STERLING_URL:-}"
tag_override_set=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -t|--tag)
      tag="${2:-}"
      shift 2
      ;;
    -r|--repo)
      repo="${2:-}"
      shift 2
      ;;
    -a|--asset)
      asset="${2:-}"
      shift 2
      ;;
    -u|--url)
      url="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
    *)
      if [[ -z "$tag_override_set" ]]; then
        tag="$1"
        tag_override_set="1"
        shift
      else
        echo "Unexpected argument: $1" >&2
        usage
        exit 1
      fi
      ;;
  esac
done

if [[ -z "$url" ]]; then
  if [[ -z "$repo" || -z "$asset" ]]; then
    echo "Missing repo/asset values." >&2
    usage
    exit 1
  fi

  if [[ -z "$tag" || "$tag" == "latest" ]]; then
    tag="$(get_latest_tag "$repo")"
    if [[ -z "$tag" ]]; then
      echo "Unable to resolve latest release tag." >&2
      exit 1
    fi
  fi

  url="https://github.com/${repo}/releases/download/${tag}/${asset}"
fi

if ! command -v unzip >/dev/null 2>&1; then
  echo "Missing required tool: unzip" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_DIR="${SCRIPT_DIR}/build"

if [[ -z "$TARGET_DIR" || "$TARGET_DIR" == "/" ]]; then
  echo "Refusing to operate on invalid target directory." >&2
  exit 1
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT
zip_path="${tmp_dir}/sterling-forge.zip"
extract_dir="${tmp_dir}/extract"
mkdir -p "$extract_dir"

echo "Downloading: $url"
if command -v curl >/dev/null 2>&1; then
  curl -fsSL -o "$zip_path" "$url"
elif command -v wget >/dev/null 2>&1; then
  wget -O "$zip_path" "$url"
else
  echo "Missing required tool: curl or wget" >&2
  exit 1
fi

unzip -q "$zip_path" -d "$extract_dir"

source_dir=""
if [[ -d "${extract_dir}/dist" ]]; then
  source_dir="${extract_dir}/dist"
elif [[ -d "${extract_dir}/build" ]]; then
  source_dir="${extract_dir}/build"
else
  candidate="$(find "$extract_dir" -maxdepth 2 -type d \( -name dist -o -name build \) | head -n 1)"
  if [[ -n "$candidate" ]]; then
    source_dir="$candidate"
  else
    source_dir="$extract_dir"
  fi
fi

echo "Replacing ${TARGET_DIR} with contents from ${source_dir}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
cp -R "${source_dir}/." "$TARGET_DIR/"

echo "Done."
