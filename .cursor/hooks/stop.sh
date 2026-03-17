#!/usr/bin/env bash
set -euo pipefail

INPUT="$(cat || true)"

# Compatibility: ignore recursive stop hooks (Claude-style payloads)
if command -v jq >/dev/null 2>&1; then
  if [ "$(printf '%s' "$INPUT" | jq -r '.stop_hook_active // false' 2>/dev/null || echo false)" = "true" ]; then
    exit 0
  fi
fi

PROJECT_DIR="${CURSOR_PROJECT_DIR:-${CLAUDE_PROJECT_DIR:-$(pwd)}}"
cd "$PROJECT_DIR" || exit 1

for cmd in \
  "cd vdom && lamdera make --optimize elm/Main.elm --output=/dev/null" \
  "cd vdom && elm-test --compiler=lamdera"
do
  if ! output="$(bash -lc "$cmd" 2>&1)"; then
    if command -v jq >/dev/null 2>&1; then
      jq -n --arg reason "$output" \
        '{continue: true, permission: "deny", user_message: $reason, agent_message: $reason}'
    else
      printf '%s\n' '{"continue":true,"permission":"deny","user_message":"stop hook failed; install jq to see details"}'
    fi
    exit 2
  fi
done

exit 0

