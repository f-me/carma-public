#!/bin/bash -e

# B-prime VIN automatic import script.
#
# Usage:
#
#     ./bprime-vin-import.sh
#
# Must be run from ~carma directory.
#
# sftp(1) and sshpass(1) must be available.
#
# .netrc file must contain SFTP credentials for the host in one line.

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

# CaRMa Postgres connection info
PG="localhost,5432,carma_db_sync,pass,carma"

DIR="incoming"

# Absolute path to vinnie executable
VINNIE="${HOME}/projects/carma/srv/.cabal-sandbox/bin/vinnie"

TMPDIR=$(mktemp -d /tmp/bprime.`date +%F`.XXXXXX)

# Download VIN databases
echo "get -r ${DIR}/ ${TMPDIR}" | sshpass -p $(grep ${HOST} ~/.netrc | cut -d' ' -f6) sftp -P ${PORT} ${USER}@${HOST}

# Process all downloaded files
for IN in "${TMPDIR}/${DIR}"/*
do
    # Unpack single file from the archive
    OUT="${IN%csv}OUT.csv"

    # Delete blanks
    sed -i '/^[[:blank:]]*$/d' "${IN}"

    # Dump result file name and vinnie output
    echo "${OUT}"

    # Run vinnie
    ${VINNIE} -c ${PG} \
         -s ${SUBPROGRAM} \
         "${IN}" "${OUT}" ${COMMITTER} ${FORMAT}
done
