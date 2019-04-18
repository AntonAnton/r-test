install.packages("devtools")
library(devtools)
install_github("google/amss")


library(amss)
n.years <- 4
time.n <- n.years * 52

# параметр activity отвечает за готовность потребителя купить продукт данной категории
#3 состояния - соотношение - inactive(не готов), exploratory(исследует рынок), active(готов к покупке) - в сумме 1

activity.transition <- matrix(  # матрица перехода готовности потребителя купить товар без медиа воздействия
  c(0.60, 0.30, 0.10,
    0.60, 0.30, 0.10,
    0.60, 0.30, 0.10),
  nrow = length(kActivityStates), byrow = TRUE)
# матрица для периода указывается


# favorability - отношение потребителя к бренду
# unaware(не знает/не слышал), negative(отриц), neutral, somewhat favorable, favorable(позитивное)

favorability.transition <- matrix(
  c(0.03, 0.07, 0.65, 0.20, 0.05,  # migration from the unaware state
    0.03, 0.07, 0.65, 0.20, 0.05,  # negative state
    0.03, 0.07, 0.65, 0.20, 0.05,  # neutral state
    0.03, 0.07, 0.65, 0.20, 0.05,  # somewhat favorable state
    0.03, 0.07, 0.65, 0.20, 0.05),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)


# a sinusoidal pattern
market.rate.nonoise <-
  SimulateSinusoidal(n.years * 52, 52,
                     vert.trans = 0.6, amplitude = 0.25)
# with some added noise
market.rate.seas <- pmax(
  0, pmin(1,
          market.rate.nonoise *
            SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0.3)))
# выше - строим график продаж (сезонный - в данном случае сезон - неделя, разбавлен "шумом")


# инилизационные параметры поведения потребителей
nat.mig.params <- list(
  population = 2.4e8, # количество потребителей
  market.rate.trend = 0.68, # доля людей от 1, которые потенциально рассматриваются как готовые купить товар данной категории
  market.rate.seas = market.rate.seas,
  # activity states for newly responsive (in-market & un-satiated)
  prop.activity = c(0.375, 0.425, 0.2), # вектор указываеющий изначальное распределение людей, готовых к покупке 
  # brand favorability, initial proportions.
  prop.favorability = c(0.03, 0.07, 0.65, 0.20, 0.05), # вектор, задающий начальное отношение потребителей к бренду
  # everyone is a switcher
  prop.loyalty = c(1, 0, 0), # лояльность к бренду (нейтральный(переключаемый они называют его), -, +)
  transition.matrices = list(
    activity = activity.transition,
    favorability = favorability.transition))


# распределение бюджетам по неделям на весь срок(указываютя доли бюджета на каждую неделю)
budget.index <- rep(1:n.years, each = 52)

# распределение бюджета на теле медиа компанию
tv.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -0.7, 0.7, -0.7))
tv.flighting <- tv.flighting[c(6:length(tv.flighting), 1:5)]


# аналогично как выше, только зиначально берутся параметры что люди не готовы ничего покупать, а в результате компнии все готовы купить
tv.activity.trans.mat <- matrix(
  c(1.00, 0.00, 0.00,  # migration originating from the inactive state
    0.00, 1.00, 0.00,  # exploratory state
    0.00, 0.00, 1.00),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)

# аналогично,начальное какое-то распределение, коорое превращается в то что все 100% лояльны к бренду
tv.favorability.trans.mat <- matrix(
  c(0.4,  0.0,  0.4, 0.2, 0.0,  # migration from the unaware state
    0.0,  0.9,  0.1, 0.0, 0.0,  # negative state
    0.0,  0.0,  0.6, 0.4, 0.0,  # neutral state
    0.0,  0.0,  0.0, 0.8, 0.2,  # somewhat favorable state
    0.0,  0.0,  0.0, 0.0, 1.0),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

# устанавливаем параметры ТВ медиа компании

params.tv <- list(
  audience.membership = list(activity = rep(0.4, 3)), # распределение людей по показателям «активность», «благосклонность», «лояльность» и «доступность» 
  budget = rep(c(545e5, 475e5, 420e5, 455e5), length = n.years), # бюджет по годам
  budget.index = budget.index, # шаблон распределения бюджета (общего)
  flighting = tv.flighting, # распредение бюджета на рекламу
  unit.cost = 0.005, # цена за 1рекламу
  hill.ec = 1.56, 
  hill.slope = 1, # 2 параметра, опредедающие успешность, но пока не понял что они значат, нужно углубиться в экономическую сторону
  transition.matrices = list(
    activity = tv.activity.trans.mat,
    favorability = tv.favorability.trans.mat))

# цена за клик для бюджета поисковых медиа
cpc.min <- 0.8
cpc.max <- 1.1


# uncapped spend, shut off the first 2 of every 13 weeks
# функция от времени, коорая вычисляет бюджет для конкретного момента времени
spend.cap.fn <- function(time.index, budget, budget.index) {
  if ((time.index %% 13) > 1) {
    return(Inf)
  } else {
    return(0)
  }
}



# ф-я вычисления цены за клик в зависимости от распределения бюджета
bid.fn <- function(time.index, per.capita.budget, budget.index) {
  return(1.1)
}


# функция от времени, выдающая бюджеты для разных ключевых поисковых выражений
kwl.fn <- function(time.index, per.capita.budget, budget.index) {
  return(4.5 * per.capita.budget)
}


search.activity.trans.mat <- matrix(
  c(0.05, 0.95, 0.00,  # starting state: inactive
    0.00, 0.85, 0.15,  # starting state: exploratory
    0.00, 0.00, 1.00),  # starting: purchase
  nrow = length(kActivityStates), byrow = TRUE)
search.favorability.trans.mat <- matrix(
  c(1.0, 0.0, 0.0, 0.0, 0.0,  # unaware
    0.0, 1.0, 0.0, 0.0, 0.0,  # negative
    0.0, 0.0, 1.0, 0.0, 0.0,  # neutral
    0.0, 0.0, 0.0, 1.0, 0.0,  # favorable
    0.0, 0.0, 0.0, 0.0, 1.0),  # loyal
  nrow = length(kFavorabilityStates), byrow = TRUE)


# ввод паретров для поисковых медиа
params.search <- list(
  audience.membership = list(activity = c(0.01, 0.3, 0.4)),
  budget = (2.4e7 / n.years) * (1:n.years),
  budget.index = budget.index,
  spend.cap.fn = spend.cap.fn,
  bid.fn = bid.fn,
  kwl.fn = kwl.fn,
  query.rate = 1,
  cpc.min = cpc.min,
  cpc.max = cpc.max,
  ctr = list(activity = c(0.005, 0.08, 0.10)), # распределение кликов по группам  ’activity’, ’favorability’, ’loyalty’, and ’availability’ - установка того чего хотим добиться 
  relative.effectiveness = c(0, 0.1, 1), # эффективность переходов на сайт для простых переходов, переходов с рекламы с бесплатныи кликом и с платным кликом
  transition.matrices = list(
    activity = search.activity.trans.mat,
    favorability = search.favorability.trans.mat))

# чтобы лучше понять этот раздел я думаю нужно в гугле попробовать настроить таргетированную рекламу


# задаем параметры для продаж
sales.params <- list(
  competitor.demand.max = list(loyalty = c(0.8, 0, 0.8)), # список числовых векторов, соответствующих 
  #каждому состоянию бренда (благосклонность, лояльность и доступность) к товару конкурентов (в каком случае потребитель выберет конкурента если товар рекламодателя для него слишком дорогой) 
  # в данном случае лояльность, т е есть распределение людей по пказателям лояльности(нейтральные, негатив, позитив), данные числа указывают вероятность того что чеовек из данной группы людей купит товар при условиях... (выше)
  advertiser.demand.slope = list(favorability = rep(0, 5)),
  # каждому состоянию бренда (благосклонность, лояльность и доступность)
  # функция, показывающая верояность того что покупатель, готовый к покупке откажется от нее при возрастании цены если нет конкурентов
  advertiser.demand.intercept = list(
    favorability = c(0.014, 0, 0.2, 0.3, 0.9)),
  # каждому состоянию бренда (благосклонность, лояльность и доступность)
  # для каждой группы потребителей вероятность того что товар будет куплен при средней цене и при отсутсвии конкурентов
  price = 80)


# запускаем сам симулятор
sim.data <- SimulateAMSS(
  time.n = time.n,
  nat.mig.params = nat.mig.params,
  media.names = c("tv", "search"),
  media.modules = c(
    `DefaultTraditionalMediaModule`,
    `DefaultSearchMediaModule`),
  media.params = list(params.tv, params.search),
  sales.params = sales.params)


burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51

observed.data <- sim.data$data

write.csv(sim.data$data, file = "testData.csv")
write.csv(sim.data$dataюагдд, file = "testDataFull.csv")
