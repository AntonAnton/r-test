# Рекламодателем тут называется "Наша" компания, для которой ведутся подсчеты

#' @param price - вектор с ценами товара рекламодателя, если значений меньше, то цены повторяются, можно константу задавать


#' @param mean.price - средняя цена по рынку относительно которой ведется отсчет


# Все параметры ниже векторы векторы с вероятностями для каждого состояния бренда (favorability, loyalty, and availability)
# Для групп потребителей с состоянием "Покупка"

#' @param advertiser.demand.intercept
# это вероятность того, что потребители в этом сегменте купят продукт рекламодателя, если цена будет mean.price. 

# если меньше 1, то покупатели либо уйдут к конкуренту либо не купят ничего. при уменьшении цены количество продаж будет увелилчиваться со скоростю advertiser.demand.slope


#' @param advertiser.demand.slope 
# вероятности того, что потребители в этом сегменте купят продукт рекламодателя,
#  когда цена возрастет на 1, когда нет конкуренции.


#' @param competitor.demand.max 
# вероятность того, что потребители в этом сегменте купят продукт конкурента, 
# когда продукт рекламодателя слишком дорог, чтобы покупатель рассматривал его как вариант покупки

#  если меньше 1 то при высокой цене будут люди, которые не купят ничего


#' @param competitor.demand.replacement 
# показатель зависимостри влияния цены на то что пользователи выберут товар конкурента
#  1 - продажи конкурентов не зависят от цен рекламодателя, и продажи конкурентов в максимально возможной степени заменяют продажи рекламодателя. 
#  0 -  продажи рекламодателя не зависят от присутствия конкурента, и продажи рекламодателя заменяют продажи конкурента в максимально возможной степени
# (покупатели будут вибирать товар рекламодателя вне зависимости от ценовых политик конкурентов). 
# Т. е. чем ближе показатель к 0,5 тем больше покупатели будут ориентироваться на цену товара
#   \ code {list (loyalty = c (0.5, 0.1, 0.9)} - switcher, loyal, competitor-loyal - они предлагают как стандартнаю можель поведения потребителя


#' @param purchase.quantity.intercept - шт
# среднее количество товаров, купленны пользователям за покупку ессли цена равна mean.price
# >=1


#' @param purchase.quantity.slope - шт
#  число, показывающее уменьшение среднего количества товаров на одного потребителя, 
# покупающего у бренда рекламодателя, с учетом увеличения цены на единицу.
# >=0


#' @param purchase.quantity.competitor  - шт
# среднее количество товаров в покупке если покупатель покупает товары конкурента


#' @param unit.cost - шт 
# себестоимость товара


#' @param advertiser.transitions 
# матрицы перехода для показателей 'favorability', 'loyalty', and 'availability'
# матрицы перехода отпределяющие изменение отношения людей к бренду рекламоателя после покупки

#' @param competitor.transitions 
# то же самое что выше, только для покупателей продукции конкурента


# поля pop - люди которые остались без покупки
# pop.out - которые ушли к конкуренту.