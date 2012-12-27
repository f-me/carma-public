@echo off
set to=resources/static/js/gen/
set from=resources/static/coffee/

:: you can add custom keys to coffee compiler, like
:: ./coffee-build.hs -wl
:: so it will run watch on FROM and jslint after compile
coffee -c -o %to% %from%
