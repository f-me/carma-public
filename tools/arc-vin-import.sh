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
#
# .netrc file must contain SFTP credentials for ARC host in one line.

if [ $# -ne 2 ]
then
    echo "Usage: ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID"
    exit 1
fi


# CONFIGURATION:

# ARC VinFormat id
FORMAT="1000"
# Contract commiter (PSA user)
COMMITTER="387"

# ARC host data
USER="ramc"
HOST="arcftp.arceurope.com"
DIR="Production/Vehicle_info/Common"

# CaRMa Postgres connection info
PG="localhost,5432,carma_db_sync,pass,carma"

# Report mail parameters
MAIL_FROM="carma@carma.ruamc.ru"
MAIL_TO=("robots@formalmethods.ru" "Alexander.Dimakov@ruamc.ru" "Pavel.Golovnin@ruamc.ru")
MAIL_SUBJECT="Отчёт о загрузке контрактов PSA `date +%F`"

# END OF CONFIGURATION


NAME="$1"
SUBPROGRAM="$2"

# Absolute path to vinnie executable
VINNIE="${HOME}/carma/srv/.cabal-sandbox/bin/vinnie"

TMPDIR=$(mktemp -d /tmp/arc.`date +%F`.XXXXXX)
TMP="${TMPDIR}/${NAME}"

# Build mail message body as files are processed
MESSAGE="${TMPDIR}/message"

echo "${TMPDIR}" >> "${MESSAGE}"
echo >> "${MESSAGE}"

# Download VIN database
echo "get ${DIR}/${NAME} ${TMP}" | sshpass -p $(grep ${HOST} ~/.netrc | cut -d' ' -f6) sftp ${USER}@${HOST}

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
