#!/bin/bash -e

# B-prime VIN automatic import script.
#
# Usage:
#
#     ./bprime-vin-import.sh
#
# Fetch all files from an FTP directory and feed them to vinnie,
# mailing a report afterwards.
#
# sendemail(1), sftp(1) and sshpass(1) must be available.
#
# .netrc file must contain SFTP credentials for the host in one line.


# CONFIGURATION:

# VinFormat id
FORMAT="1067"

# Contract commiter
COMMITTER="912"

# Target subprogram
SUBPROGRAM="317"

# SFTP host data
USER="ramk"
HOST="sftp.metatrak.it"
PORT="9800"
DIR="incoming"

# CaRMa Postgres connection info
PG="localhost,5432,carma_db_sync,pass,carma"

# Report mail parameters
MAIL_FROM="carma@carma.ruamc.ru"
MAIL_TO=("robots@formalmethods.ru")
MAIL_SUBJECT="Отчёт о загрузке контрактов B-Prime `date +%F`"

# END OF CONFIGURATION


# Absolute path to vinnie executable
VINNIE="${HOME}/carma/srv/.cabal-sandbox/bin/vinnie"

TMPDIR=$(mktemp -d /tmp/bprime.`date +%F`.XXXXXX)

# Download VIN databases
echo "get -r ${DIR}/ ${TMPDIR}" \
  | sshpass -p $(grep "${HOST}.*${USER}" ~/.netrc | cut -d' ' -f6) \
    sftp -P ${PORT} ${USER}@${HOST}

# Build mail message body as files are processed
MESSAGE="${TMPDIR}/message"

echo "${TMPDIR}" >> "${MESSAGE}"
echo >> "${MESSAGE}"

# Process all downloaded files
for IN in "${TMPDIR}/${DIR}"/*
do
    # Unpack single file from the archive
    OUT="${IN%csv}OUT.csv"

    # Delete blanks
    sed -i '/^[[:blank:]]*$/d' "${IN}"

    # New section in message
    echo $(basename "${IN}")":" >> "${MESSAGE}"

    set +e
    # Run vinnie and collect output
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
    echo >> "${MESSAGE}"
done

sendemail -f "${MAIL_FROM}" \
          -t "${MAIL_TO[@]}" \
          -u "${MAIL_SUBJECT}" \
          -o message-file="${MESSAGE}" \
          -a "${ATTACHMENTS[@]}"
