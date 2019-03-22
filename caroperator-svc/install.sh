#!/usr/bin/env bash

set -e

echo === Install nginx config
cp conf/nginx.conf /etc/nginx/vhosts/caroperator-svc.conf
/etc/init.d/nginx force-reload

echo === Install rsyslog config
cp conf/syslog.conf /etc/rsyslog.d/40-caroperator-svc.conf
service rsyslog restart

echo === Install upstart job
init-checkconf conf/upstart.conf
cp conf/upstart.conf /etc/init/caroperator-svc.conf
service caroperator-svc start
