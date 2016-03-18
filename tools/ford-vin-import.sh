#!/bin/bash -e

# Ford ARC VIN automatic import script.
#
# Usage:
#
#     ./ford-vin-import.sh DATE-OF-FILE
#
# Fetch a file from the FTP host, feed it to vinnie,
# and mail a report afterwards.
#
# sendemail(1), sftp(1), unzip(1) and sshpass(1) must be available.
#
# .netrc file must contain SFTP credentials for ARC host in one line.

if [ $# -ne 1 ]
then
    echo "Usage: ./ford-vin-import.sh YYYYMMDD"
    exit 1
fi

# CONFIGURATION:

# Ford VinFormat id
FORMAT="2000"
# Contract commiter (Ford user)
COMMITTER="1192"
SUBPROGRAM="15" # Ford new

# ARC host data
USER="FordSollers"
HOST="arcftp.arceurope.com"

# CaRMa Postgres connection info
PG="localhost,5432,carma_db_sync,pass,carma"

# Report mail parameters
MAIL_FROM="carma@carma.ruamc.ru"
MAIL_TO=("robots@formalmethods.ru")
MAIL_SUBJECT="Отчёт о загрузке контрактов Ford `date +%F`"

# END OF CONFIGURATION

DATE=$1

# Absolute path to vinnie executable
NAME="CB_fordsara_${DATE}_RU.csv"
VINNIE="${HOME}/.local/bin/vinnie"

TMPDIR=$(mktemp -d /tmp/ford`date +%F`.XXXXXX)

# Build mail message body as files are processed
MESSAGE="${TMPDIR}/message"

echo "${TMPDIR}" >> "${MESSAGE}"
echo >> "${MESSAGE}"

# Download VIN database
echo "get ${NAME} ${TMPDIR}" \
  | sshpass -p $(grep "${HOST}.*${USER}" ~/.netrc | cut -d' ' -f6) \
    sftp -C ${USER}@${HOST}

IN="${TMPDIR}/${NAME}"
OUT="${IN%csv}.OUT.csv"

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

