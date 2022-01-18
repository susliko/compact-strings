## 3.2 Однотипные данные

### Задача

### Алгоритм решения
#### Наблюдения 
1. Каждый шаблон можно хранить однажды, а в компактном представлении сообщения на него ссылаться (например, по индексу в списке шаблонов).
2. Бинарный формат, в который будем сериализовать сообщения, не зависит от структуры данных и является набором функций `X => Array[Byte]`, где `X` один из:
    - примитивный тип,
    - тип-сумма,
    - тип-продукт.
   Однажды определив их, можно будет кодировать любые данные.
3. По необходимости можно применять lossless алгоритмы сжатия.

#### Бинарный формат
При десериализации важно понимать, какие байты относятся к каким полям исходной структуры.
Возможные варианты:
- Разделение данных зарезервированным разделителем 
- Запись размера данных перед началом данных. Причем размер региона, хранящего размер фиксирован
Поскольку данные у нас произвольные, первый вариант будет ошибаться на данных, содержащих зарезервированный разделитель. 
Прибегнем ко второму варианту: в качестве гипер-параметра `delimSize` алгоритма выделим размер региона, хранящего размер поля. 
От его значения зависят максимальный допустимый размер поля и оверхед сериализованного значения по сравнению с чистыми данными:
|delimSize (байты)| Максимальный размер поля (байты)|
|---|---|
|1|255|
|2|65025|
|3|16.6⋅10^6|
|4|4228⋅10^6|

Оверхед считается как `delimSize * numOfFields`

#### Сжатие
Поскольку имеем дело со строковыми данными с неравномерным использованием символов алфавита (натуральный язык), имеет смысл применить сжатие через [коды Хаффмана](https://ru.wikipedia.org/wiki/%D0%9A%D0%BE%D0%B4_%D0%A5%D0%B0%D1%84%D1%84%D0%BC%D0%B0%D0%BD%D0%B0)

### Тестирования и запуск
Для запуска необходим [sbt](https://sdkman.io/sdks#sbt)

Запуск тестов:
```
sbt test
```

Запуск примера:
```
sbt run
```

