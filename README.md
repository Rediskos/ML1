# ML1

Это репозиторий с алгоритмами машинного обучения

## Оглавление
0. [Метрические алгоритмы](#Метрические-алгоритмы-классификации)
    1. [Алгоритм ближайших соседей](#Алгоритм-ближайших-соседей)

## Метрические алгоритмы классификации
### Алгоритм ближайших соседей
Алгоритм ближайших соседей далее будет называться ***kNN***(k-nearest-neighbourhoods).
Для иллюстрации примеров используем выборку цветков ириса Фишера разбитую по классам(цветам).

![ХДЕ?????](kNN/base.png?raw=true "Optional Title")

**kNN** для каждого поступающего объёкта на классификацию берёт ***k*** ближайших соседей, и тот класс(цвет), которого оказалось больше, и будет ответом ***kNN***.
Например вот карта классификации при ***k = 5***. 
![ХДЕ?????](kNN/heatmapk5.png?raw=true "Optional Title")

Понятно, что разные ***k*** будут давать разную точность классификации, и алгоритм подбора лучшего ***k*** называется алгоритмом скользящего контроля один из которых ***LOO***(Leave-one-out).

![ХДЕ?????](kNN/LOOkNN.png?raw=true "Optional Title")

После приминения ***LOO*** на выборке ирисов Фишера обнаружилось несколько лучших ***k*** равных ***7*** и ***26***.

![ХДЕ?????](kNN/kNN_LOO_Map.png?raw=true "Optional Title")

В методе ***kNN*** все соседи, в общем случае, равноправны, однако вполне возможно ввести весовую функцию такую, что чем дальше сосед по порядку близости, тем менее он важен.
Сам метода называется методом взвешенных ближайших соседей или ***kwNN***. Т.е. вес у каждого соседа будет 
<img src="https://bit.ly/3nLqw0l" align="center" border="0" alt="q^i, q < 0" width="71" height="24" />, где *i* - номер соседа по порядку.

![ХДЕ?????](kNN/heatmapk5q07kwnn.png?raw=true "Optional Title")

Конечно для ***kwNN*** также возможно использовать ***LOO***, для нахождения лучшего ***k*** и ***q***. Можно по отдельности или для лучшего результата искать их одновременно.

![ХДЕ?????](kNN/LOOqAfterLOOk.png?raw=true "Optional Title")


![ХДЕ?????](kNN/heatmapk7q01kwnn.png?raw=true "Optional Title")

Сравнивая ***kNN*** с ***kwNN*** 


[:arrow_up:Оглавление](#Оглавление)
