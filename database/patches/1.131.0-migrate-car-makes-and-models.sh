#!/bin/bash -e

$PSQL -f baseline/5-views/2-services-view.sql
$PSQL -f baseline/5-views/3-ru-calls.sql
$PSQL -f baseline/5-views/5-ru-partners.sql
$PSQL -f baseline/5-views/7-ru-services.sql

$PSQL -t -c "select concat('hmset call:', id, ' carMake \"', carMake, '\" carModel \"', carModel, '\"') from calltbl" \
  | redis-cli > /dev/null

$PSQL -t -c "select concat('hmset case:', id, ' car_make \"', car_make, '\" car_model \"', car_model, '\"') from casetbl" \
  | redis-cli > /dev/null

$PSQL -t -c "select concat('hmset rent:', id, ' rentedMake \"', rentedMake, '\" rentedModel\"', rentedModel, '\"') from renttbl" \
  | redis-cli > /dev/null

$PSQL -t -c "select concat('hset partner:', id, ' makes \"', makes::text, '\"') from partnertbl" \
  | sed -e 's/[{}]//g' \
  | redis-cli > /dev/null
