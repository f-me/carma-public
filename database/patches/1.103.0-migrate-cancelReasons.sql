UPDATE servicetbl
SET clientCancelReason='Неисправность самоустранилась'
WHERE clientCancelReason='ccr1';

UPDATE servicetbl
SET clientCancelReason='Не устроило время ожидания'
WHERE clientCancelReason='ccr2';

UPDATE servicetbl
SET clientCancelReason='Изменились планы'
WHERE clientCancelReason='ccr3';

UPDATE servicetbl
SET clientCancelReason='Не устроили условия ремонта у дилера'
WHERE clientCancelReason='ccr4';

UPDATE servicetbl
SET clientCancelReason='Сам нашел эвакуатор/техпомощь'
WHERE clientCancelReason='ccr5';

UPDATE servicetbl
SET clientCancelReason='Сам решил проблему транспортировки'
WHERE clientCancelReason='ccr6';

UPDATE partnercanceltbl
SET partnerCancelReason='Занято (Постоянно короткие гудки)'
WHERE partnerCancelReason='pcr1';

UPDATE partnercanceltbl
SET partnerCancelReason='Нет свободных эвакуаторов'
WHERE partnerCancelReason='pcr2';

UPDATE partnercanceltbl
SET partnerCancelReason='Время подачи эвакуатора больше часа'
WHERE partnerCancelReason='pcr3';

UPDATE partnercanceltbl
SET partnerCancelReason='Подрядчик не предоставляет данную услугу'
WHERE partnerCancelReason='pcr4';

UPDATE partnercanceltbl
SET partnerCancelReason='Не работают в данное время'
WHERE partnerCancelReason='pcr5';

UPDATE partnercanceltbl
SET partnerCancelReason='Эвакуатор не может взять такой автомобиль (VW Crafter, Peugeot Boxer, Ford Transit)'
WHERE partnerCancelReason='pcr6';

UPDATE partnercanceltbl
SET partnerCancelReason='Эвакуатор сломан'
WHERE partnerCancelReason='pcr7';

UPDATE partnercanceltbl
SET partnerCancelReason='Телефон не отвечает'
WHERE partnerCancelReason='pcr8';

UPDATE partnercanceltbl
SET partnerCancelReason='Другое'
WHERE partnerCancelReason='pcr9';

UPDATE partnercanceltbl
SET partnerCancelReason='Нет свободных машин техпомощи'
WHERE partnerCancelReason='pcr10';
