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

Понятно, что разные ***k*** будут давать разную точность классификации, и алгоритм скользящего контроля для подбора лучшего ***k*** называется  ***LOO***(Leave-one-out).

![ХДЕ?????](kNN/LOOkNN.png?raw=true "Optional Title")

После приминения ***LOO*** на выборке ирисов Фишера обнаружилось несколько лучших ***k*** равных ***7*** и ***26***. Но

[:arrow_up:Оглавление](#Оглавление)
