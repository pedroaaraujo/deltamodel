#!/usr/bin/env bash
set -euo pipefail
project_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
report_dir=$(mktemp -d "${TMPDIR:-/tmp}/deltamodel-podman.XXXXXX")
pod_name="$(basename "$report_dir")"
runner_image=localhost/deltamodel-migration-tests:bookworm
postgres_image=${POSTGRES_IMAGE:-docker.io/library/postgres:15.6}
mysql_image=${MYSQL_IMAGE:-docker.io/library/mysql:8.4}
legacy_image=${FIREBIRD_LEGACY_IMAGE:-docker.io/jacobalberty/firebird:v2.5.9-sc}
firebird_image=${FIREBIRD_IMAGE:-docker.io/firebirdsql/firebird:5.0.4-bookworm}
created=0
cleanup() {
  if [[ "$created" == 1 ]]; then
    for backend in postgres mysql firebird firebird25; do
      if podman container exists "$pod_name-$backend"; then
        podman logs "$pod_name-$backend" >> "$report_dir/$backend-server.log" 2>&1 || true
      fi
    done
    podman pod rm -f "$pod_name" >/dev/null
  fi
  echo "Test logs: $report_dir"
}
trap cleanup EXIT
podman build -t "$runner_image" "$project_dir/tests/podman" > "$report_dir/build.log" 2>&1
podman pod create --name "$pod_name" >/dev/null
created=1
# Disposable credentials; no ports published, no existing databases mounted.
podman run --rm -d --pod "$pod_name" --name "$pod_name-postgres" \
  -e POSTGRES_USER=dm -e POSTGRES_PASSWORD=dm_test_only -e POSTGRES_DB=dm "$postgres_image" >/dev/null
podman run --rm -d --pod "$pod_name" --name "$pod_name-mysql" \
  -e MYSQL_USER=dm -e MYSQL_PASSWORD=dm_test_only -e MYSQL_ROOT_PASSWORD=dm_test_only -e MYSQL_DATABASE=dm "$mysql_image" >/dev/null
podman run --rm -d --pod "$pod_name" --name "$pod_name-firebird" \
  -e FIREBIRD_ROOT_PASSWORD=dm_test_only -e FIREBIRD_DATABASE=dm.fdb "$firebird_image" >/dev/null
podman image inspect "$runner_image" "$postgres_image" "$mysql_image" "$firebird_image" \
  --format '{{.RepoTags}} {{.Id}} {{.RepoDigests}}' > "$report_dir/images.txt"
podman run --rm --pod "$pod_name" --security-opt label=disable \
  -v "$project_dir:/workspace:ro" "$runner_image" sh tests/podman/run-inside.sh 2>&1 | tee "$report_dir/tests.log"

# Exercise the legacy branch on its real server, reusing the isolated pod port.
podman logs "$pod_name-firebird" > "$report_dir/firebird-server.log" 2>&1
podman stop "$pod_name-firebird" >/dev/null
podman run --rm -d --pod "$pod_name" --name "$pod_name-firebird25" \
  -e ISC_PASSWORD=dm_test_only -e FIREBIRD_DATABASE=dm.fdb "$legacy_image" >/dev/null
podman image inspect "$legacy_image" --format '{{.RepoTags}} {{.Id}} {{.RepoDigests}}' >> "$report_dir/images.txt"
podman run --rm --pod "$pod_name" --security-opt label=disable \
  -e TEST_BACKENDS=firebird -e FIREBIRD_TEST_URL='firebird://SYSDBA:dm_test_only@127.0.0.1:3050//firebird/data/dm.fdb' \
  -v "$project_dir:/workspace:ro" "$runner_image" sh tests/podman/run-inside.sh 2>&1 | tee "$report_dir/legacy-tests.log"
