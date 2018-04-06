
Через этот сервис товарищи из caroperator.ru добавляют в БД новые контракты.

Спецификация: https://docs.google.com/document/d/1nsgjiUg2UKklqpV3byJhTVqgCxo8BM6BtsUCxsEv1lo/edit

Тикет: https://github.com/f-me/carma/issues/2548 (Formal Methods)


Необходимая инфраструктура
--------------------------

  - db user
  - carma user

Утилиты
-------

  - `sudo ./install.sh` − устанавливает конфигурационные файлы и перезапускает
    соотвтетсвующие сервисы:
    + upstart job
    + syslog
    + nginx
  - `sudo ./cert.sh init-ca` − инициализирует хранилище сертификатов для
    аутентификации клиентов по ключу


TODO
----

  - nginx `rate_limit`
