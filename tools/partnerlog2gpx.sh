#!/bin/bash

# Produce GPX tracks from partner coordinate update logs.
#
# Usage:
#
# $ grep 'partner/3298' db.log > out
# $ partnerlog2gpx.sh out > geo.gpx

echo '<?xml version="1.0" encoding="UTF-8"?>'
echo '<gpx><trk><trkseg>'
grep coords $1 | sed -e 's/\//-/g' | sed -r -e 's/(.{8}) (.{8}) .+"coords":"([[:digit:].]+),([[:digit:].]+)".+/<trkpt lon="\3" lat="\4"><time>\1T\2Z<\/time><\/trkpt>/'
echo '</trkseg></trk></gpx>'
