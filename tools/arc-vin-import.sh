#!/bin/bash -e

# ARC VIN automatic import script.
#
# Usage:
#
#     ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID
#
# Fetch a file from the FTP host, unzip it and feed the file inside to
# vinnie, mailing a report afterwards.
#
# sendemail(1), sftp(1), unzip(1) and sshpass(1) must be available.
# "carma-configurator" must be presented in $PATH.
#
# .netrc file must contain SFTP credentials for ARC host in one line.

if [ $# -ne 2 ]
then
    echo "Usage: ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID"
    exit 1
fi


# CONFIGURATION:

# WARNING! "carma-configurator" must be presented in $PATH
CONFIG_JSON=$(carma-configurator carma-tools arc_vin_import)
cfg () { printf %s "$CONFIG_JSON"; }

# ARC VinFormat id
FORMAT=$(cfg | jq -r .format)
# Contract commiter (PSA user)
COMMITTER=$(cfg | jq -r .committer)

# ARC host data
USER=$(cfg | jq -r .arc.user)
HOST=$(cfg | jq -r .arc.host)
DIR=$(cfg | jq -r .arc.dir)

# CaRMa Postgres connection info
PG="$(cfg | jq -r .postgresql.host
  ),$(cfg | jq -r .postgresql.port
  ),$(cfg | jq -r .postgresql.user
  ),$(cfg | jq -r .postgresql.password
  ),$(cfg | jq -r .postgresql.db_name
  )"

# Report mail parameters
MAIL_FROM=$(cfg | jq -r .report_email.from)
IFS=';' read -ra MAIL_TO <<< "$(cfg | jq -r '.report_email.to | join(";")')"
MAIL_SUBJECT=$(cfg \
  | jq -r .report_email.subject \
  | perl -e '
      use v5.10;
      BEGIN { $x=$ARGV[0]; @ARGV=() }
      chomp($_=<>); s/%DATE%/$x/; say
    ' -- "`date +%F`")

# END OF CONFIGURATION


NAME="$1"
SUBPROGRAM="$2"

# Absolute path to vinnie executable
VINNIE="${HOME}/.local/bin/vinnie"

TMPDIR=$(mktemp -d /tmp/arc.`date +%F`.XXXXXX)
TMP="${TMPDIR}/${NAME}"

# Build mail message body as files are processed
MESSAGE="${TMPDIR}/message"

echo "${TMPDIR}" >> "${MESSAGE}"
echo >> "${MESSAGE}"

# Download VIN database
echo "get ${DIR}/${NAME} ${TMP}" \
  | sshpass -p $(grep "${HOST}.*${USER}" ~/.netrc | cut -d' ' -f6) \
    sftp ${USER}@${HOST}

# Unpack single file from the archive
NAME="${TMPDIR}/$(unzip -Z -1 ${TMP})"
unzip "${TMP}" -d ${TMPDIR}

IN="${NAME%csv}RU.csv"
OUT="${IN%csv}OUT.csv"

# Header row
head -n 1 "${NAME}" > "${IN}"
# Filter out non-RU rows
grep -E '^([^;]*;){12}RU' "${NAME}" >> "${IN}"

# New section in message
echo $(basename "${IN}")":" >> "${MESSAGE}"

set +e
# Run vinnie
${VINNIE} -c ${PG} \
          -s ${SUBPROGRAM} \
          "${IN}" "${OUT}" ${COMMITTER} ${FORMAT} >> "${MESSAGE}"
vinerr=$?
set -e

# Attach input file if critical error occured
if [ $vinerr -ne 0 ]
then
    ATTACHMENTS+=("${IN}")
fi
# Attach report if it is not empty (= non-critical errors occured)
if [ -r "${OUT}" ]
then
    ATTACHMENTS+=("${OUT}")
fi

sendemail -f "${MAIL_FROM}" \
          -t "${MAIL_TO[@]}" \
          -u "${MAIL_SUBJECT}" \
          -o message-file="${MESSAGE}" \
          -a "${ATTACHMENTS[@]}"
