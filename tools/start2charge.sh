#!/bin/bash -e



sql="select id from techtbl where techtype = 'start';"
ids=`sudo -u postgres psql carma -t -c "$sql"`

for id in $ids ; do
  curl -X PUT http://localhost:8000/_/tech/$id --data '{"techType":"charge"}'
done
