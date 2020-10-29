
#возвращает датафрейм с мат. ожиданиями
#для всех признаков по всем классам
calc_mu <- function (features) {
  #features - датафрейм: вектора признаков - фактор классов
  
  #ответ
  ans <- data.frame()
  
  #колличество признаков
  l <- dim(features)[2]
  
  for(i in levels(features[, l])){
    
    #хранит признаки рассматриваемого класса
    tmp_slice <- features[features[,l] == i, ]
    
    #колличество объектов рассматриваемого класса
    m <- dim(tmp_slice)[1]
    
    #посчитанные мат ожидания для рассматриваемого класса
    mus_for_one <- vector()
    
    for(j in 1:(l - 1)) {
      mus_for_one <- cbind(mus_for_one, sum(tmp_slice[, j]) / m)
    }
    
    ans <- rbind(ans, c(mus_for_one, i))
  }
  names(ans) <- names(features)
  
  return(ans)
}

#возвращает датафрейм средне квадратичных отклонений
#для всех признаков по всем классам
calc_sigma <- function (features) {
  #features - датафрейм: вектора признаков - фактор классов
  
  #ответ
  ans <- data.frame()
  
  #колличество признаков
  l <- dim(features)[2]
  
  for(i in levels(features[, l])){
    
    #хранит признаки рассматриваемого класса
    tmp_slice <- features[features[,l] == i, ]
    
    #колличество объектов рассматриваемого класса
    m <- dim(tmp_slice)[1]
    
    #посчитанные мат ожидания для рассматриваемого класса
    disp_for_one <- vector()
    
    for(j in 1:(l - 1)) {
      #мат ожидание в квадрате
      double_mu <- (sum(tmp_slice[, j]) / m) ^ 2
      #мат ожидание от квадрата
      mu_from_double <- (sum(tmp_slice[, j] ^ 2) / m)
      
      disp_for_one <- cbind(disp_for_one, sqrt(mu_from_double - double_mu))
    }
    
    ans <- rbind(ans, c(disp_for_one, i))
  }
  
  names(ans) <- names(features)
  return(ans)
}

#возвращает датафрейм априорных вероятностей
calc_aprior_prob <- function(features) {
  m <- dim(features)[1]
  l <- dim(features)[2]
  
  tb <- table(features[,l]) / m
  
  tb <- data.frame(tb)
  ans <- tb[, 2:1]
  names(ans) <- c("aprior", names(features)[l])
  return(ans)
}

#формула плотности для одного признака
#из класса для ннбс
formula_tight <- function(Point, mu, sigma) {
  for_exp <- (Point - mu)^2
  for_exp <- for_exp / (2 * sigma ^ 2)
  
  #правая часть формулы
  rpart <- exp(-for_exp)
  
  #левая часть формулы.
  lpart <- 1 / (sigma * sqrt(2 * pi))
  
  ans <- lpart * rpart
  
  return(ans)
  
}

#возвращаем плотность для точки
#для наивного байесовского классификатора
calc_tight <- function(feature, tclass, tmus, tsigmas) {
  #feature - признаки, как строка дата фрейма
  #tmus - датафрейм мат ожиданий
  #tsigmas - датафрейм среднеквадратичных отклонений
  #tclass - класс, проверяющийся
  
  #колличество признаков
  l <- dim(feature)[2]
  
  #выделить мат ожидание относящиеся к проеряемому классу
  tl <- dim(tmus)[2]
  mus <- tmus[tmus[, tl] == tclass, ]
  
  #выделить среднеквадратичные отклонения проверяемого класса
  tl <-dim(tsigmas)
  sigmas <- tsigmas[tsigmas[,tl] == tclass, ]
  
  
  
  #хранит суммы логарифмов плотностей
  tight_sum <- 0
  
  for(i in 1:l - 1) {
    tmp <- formula_tight(feature[,i], mus[,i], sigmas[,i])
    tight_sum <- tight_sum + log(tmp)
  }
  
  return(tight_sum)
}

#основной алгоритм наивного байеса.
naive_bias <- function(x, y, tlyambda = NA, tmu = NA, tsigma = NA, taprior = NA) {
  #x - объект для классификации
  #y - выборка
  #tmu - матожиданий предоставленные пользователем.
  #tsigma - сигмы предоставленные пользователем 
  #taprior - априорные вероятности предоставленные пользователем
  
  mu <- NA #мат ожидания
  sigma <- NA #сигмы
  aprior <- NA #априорные вероятности
  lyambda <- NA #переменные штрафа за ошибку
  
  #условия проверяющие дал ли пользователь какие-нибудь начальные данные
  #если не дал, то высчитать самостоятельно
  if(is.na(tmu) == FALSE) {
    mu <- tmu
  } else {
    mu <- calc_mu(y)
  }
  
  if(is.na(tsigma) == FALSE) {
    sigma <- tsigma 
  } else {
    sigma <- calc_sigma(y)
  }
  
  if(is.na(taprior) == FALSE) {
    aprior <- taprior
  } else {
    aprior <- calc_aprior_prob(y)
  }
  
  #использовать датафрейм мат ожиданий, чтобы посчитать колличество классов
  l <- dim(mu)[1]
  
  if (is.na(tlyambda == FALSE)) {
    lyambda <- tlyambda
  } else {
    lyambda <- seq(1, l)
  }
  
  maxv <- -1 #начальный максимальный результат
  best_class <- aprior[1,2] #счтитаем клссом по умолчанию первый класс
  
  for (i in 1:l) {
    tclass <- aprior[i,2]
    aprior_part <- log(lyambda[i] * aprior[i, 1])
    tight_part <- calc_tight(i, tclass, mu, sigma)
    
    united_part <- aprior_part + tight_part
    
    if(united_part > maxv) {
      maxv <- united_part
      best_class <- tclass
    }
  }
  
  return(best_class)
}