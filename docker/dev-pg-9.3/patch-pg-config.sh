#!/usr/bin/env bash
set -e

perl -p -i -e "
    s/^#(unix_socket_directories) = '([^']*)'/\1 = '\2'/; # for new config
    s/^(unix_socket_directories) = '[^']*'/\1 = '\/tmp'/;

    # removing duplicate in older config
    \$_ = '' if /^unix_socket_directories/ && \$unix_socket_directories++;

    # datestyle as in production config
    s/^(datestyle) = '[^']*'/\1 = 'iso,dmy'/;

    # locales as in production config
    s/^(lc_messages) = '[^']*'/\1 = 'C'/;
    s/^(lc_monetary) = '[^']*'/\1 = 'ru_RU.UTF-8'/;
    s/^(lc_numeric) = '[^']*'/\1 = 'ru_RU.UTF-8'/;
    s/^(lc_time) = '[^']*'/\1 = 'ru_RU.UTF-8'/;

    s/^(default_text_search_config) = '[^']*'/\1 = 'pg_catalog.russian'/;
" "$PGDATA/postgresql.conf"
