#!/bin/sh
set -eu
mkdir -p /tmp/deltamodel-build
for suite in test_suite test_migrations test_migrations_integration test_autoincrement test_autoincrement_integration; do
  if ! fpc -B -FE/tmp/deltamodel-build -FU/tmp/deltamodel-build -Fusrc "tests/$suite.lpr" > "/tmp/$suite-build.log" 2>&1; then
    cat "/tmp/$suite-build.log"
    exit 1
  fi
done
/tmp/deltamodel-build/test_suite
/tmp/deltamodel-build/test_migrations
/tmp/deltamodel-build/test_autoincrement
result=0
for backend in ${TEST_BACKENDS:-postgresql mysql firebird sqlite}; do
  case "$backend" in
    postgresql) export DELTAMODEL_TEST_URL='postgresql://dm:dm_test_only@127.0.0.1:5432/dm';;
    mysql) export DELTAMODEL_TEST_URL='mysql://dm:dm_test_only@127.0.0.1:3306/dm';;
    firebird) export DELTAMODEL_TEST_URL=${FIREBIRD_TEST_URL:-firebird://SYSDBA:dm_test_only@127.0.0.1:3050//var/lib/firebird/data/dm.fdb};;
    sqlite) export DELTAMODEL_TEST_URL='sqlite:///:memory:';;
  esac
  echo "BACKEND $backend"
  /tmp/deltamodel-build/test_migrations_integration || result=1
  /tmp/deltamodel-build/test_autoincrement_integration || result=1
done
exit "$result"
