UPDATE casetbl SET car_color = s.label
FROM "Colors" s WHERE car_color = s.value;
