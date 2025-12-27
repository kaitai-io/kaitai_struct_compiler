# This shell script is meant to be sourced (not executed), so it deliberately
# does not have the executable bit set and does not contain a shebang.

export GIT_COMMIT=$(git log -1 --format=%h)
export GIT_DATE_ISO=$(TZ=UTC git log -1 --date=iso-strict-local --format=%cd)
export GIT_DATE=$(TZ=UTC git log -1 --date=format-local:%Y%m%d.%H%M%S --format=%cd)
export KAITAI_STRUCT_VERSION=0.12-SNAPSHOT${GIT_DATE}.${GIT_COMMIT}

echo "KAITAI_STRUCT_VERSION=$KAITAI_STRUCT_VERSION"
