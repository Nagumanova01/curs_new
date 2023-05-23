# Code_img

# code_ris21.R
Граница для 𝜎 при 𝜋1 = 1−𝜋, 𝜋2 = 0.5, 𝜋3 = 𝜋 (красный), при 𝜋1 = 0.1, 𝜋2 = 0.5, 𝜋3 = 𝜋 (зелёный). 

Функция p2(𝜋, 𝜎) вычисляет значение веса p2 для 𝜋1 = 0.1, 𝜋2 = 0.5, 𝜋3 = 𝜋 в зависимости от 𝜋 и 𝜎.
Функция p2_sim(𝜋, 𝜎) вычисляет значение веса p2 для 𝜋1 = 1−𝜋, 𝜋2 = 0.5, 𝜋3 = 𝜋 в зависимости от 𝜋 и 𝜎.
Функция f находит верхнюю границу для 𝜎: такое значение 𝜎, при котором p2 становится отрицательным.

# code_ris21.R
Ошибка аппроксимации математического ожидания и дисперсии в зависимости от 𝜎.
Функция m(𝜎) вычисляет ошибку аппроксимации математического ожидания.
Функция s(𝜎) вычисляет ошибку аппроксимации дисперсии.

# code_ris41_42_43.R
Функция g(x) вычисляет ошибку аппроксимации медианы от sigma2 при sigma1_2 = 0.75
Функция t(x) вычисляет ошибку аппроксимации q10 от sigma2 при sigma1_2 = 0.75
Функция w(x) вычисляет ошибку аппроксимации q90 от sigma2 при sigma1_2 = 0.75

# code_ris44_45.R
Строит оценки плотности для 𝜉 и 𝜂 на одном графике для сравнения с указанием квантилей 0.1, 0.5, 0.9

# code_tab123.R
Генерирует три таблицы. 
Ошибка аппроксимации медианы (%) в зависимости от 𝜎1^2 (строка) и 𝜎2^2 (столбец) при 𝜇1 = 𝜇2 = 4. 
Ошибка аппроксимации q10 (%) в зависимости от 𝜎1^2 (строка) и 𝜎2^2 (столбец) при 𝜇1 = 𝜇2 = 4. 
Ошибка аппроксимации q90 (%) в зависимости от 𝜎1^2 (строка) и 𝜎2^2 (столбец) при 𝜇1 = 𝜇2 = 4. 

# code_tab456.R
Считает значения функции 𝐹_𝜉(𝑥) от квантилей 𝑧10, 𝑧50, 𝑧90.  
Функция G - обратная функции распределения 𝐹_𝜂(𝑥), находит 𝑧10, 𝑧50, 𝑧90. 
Значения integ10, integ50, integ90 - это 𝐹_𝜉(𝑥) от 𝑧10, 𝑧50, 𝑧90, найденная через интеграл по формуле свертки. 

# code_tab78.R
gamma3_xi - коэффициент асимметрии суммы, 
gamma3_eta - коэффициент асимметрии аппроксимации, 
gamma4_xi - коэффициент эксцесса суммы, 
gamma4_eta - коэффициент эксцесса аппроксимации.
