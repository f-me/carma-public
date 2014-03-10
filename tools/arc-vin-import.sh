#!/bin/bash -e

# ARC VIN automatic import script.
#
# Usage:
#
#     ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID
#
# Must be run from ~carma directory.
#
# cURL with SFTP support and unzip must be available.
#
# .netrc file must contain SFTP credentials.

if [ $# -ne 2 ]
then
    echo "Usage: ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID"
    exit 1
fi

FORMAT="1000"
COMMITTER="387"

NAME="$1"
SUBPROGRAM="$2"

USER="ramc"
HOST="arcftp.arceurope.com"

PG="localhost,5432,carma_db_sync,pass,carma"

DIR="/Production/Vehicle_info/Common"

VINNIE="${HOME}/carma/tools/vinnie/cabal-dev/bin/vinnie"
TMPDIR=$(mktemp -d /tmp/arcXXXXXX)

TMP="${TMPDIR}/${NAME}"

# Download VIN database
curl -k --netrc "sftp://${USER}@${HOST}${DIR}/${NAME}" > ${TMP}

# Unpack single file from the archive
NAME="${TMPDIR}/$(unzip -Z -1 ${TMP})"
unzip ${TMP} -d ${TMPDIR}

IN=${NAME%csv}RU.csv
OUT=${IN%csv}OUT.csv

# Header row
head -n 1 ${NAME} > ${IN}
# Filter out non-RU rows
grep -E '^([^;]*;){12}RU' ${NAME} >> ${IN}

# Run vinnie
${VINNIE} -c ${PG} \
          -s ${SUBPROGRAM} \
          ${IN} ${OUT} ${COMMITTER} ${FORMAT}

# Dump output and result file name
echo ${OUT}
