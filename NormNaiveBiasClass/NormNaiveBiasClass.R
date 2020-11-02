library(lattice)

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
formula_tight <- function(Point, mu, tsigma) {
  mu <- as.numeric(mu)
  tsigma <- as.numeric(tsigma)
  for_exp <- (Point - mu)^2
  for_exp <- for_exp / (2 * tsigma ^ 2)
  
  #правая часть формулы
  rpart <- exp(-for_exp)
  
  #левая часть формулы.
  lpart <- 1 / (tsigma * sqrt(2 * pi))
  
  ans <- lpart * rpart
  
  return(ans)
  
}

#возвращаем плотность для точки
#для наивного байесовского классификатора
calc_tight <- function(feature, tclass, tmus, ttsigmas) {
  #feature - признаки, как строка дата фрейма
  #tmus - датафрейм мат ожиданий
  #ttsigmas - датафрейм среднеквадратичных отклонений
  #tclass - класс, проверяющийся
  
  #колличество признаков
  l <- dim(feature)[2]
  
  #выделить мат ожидание относящиеся к проеряемому классу
  tl <- dim(tmus)[2]
  mus <- tmus[tmus[, tl] == tclass, ]
  
  #выделить среднеквадратичные отклонения проверяемого класса
  tl <-dim(ttsigmas)[2]
  tsigmas <- ttsigmas[which(ttsigmas[,tl] == tclass) , ]
  
  
  
  #хранит суммы логарифмов плотностей
  tight_sum <- 0
  
  for(i in 1:l) {
    tmp <- formula_tight(feature[,i], mus[,i], tsigmas[,i])
    tight_sum <- tight_sum + log(tmp)
  }
  
  return(tight_sum)
}

#основной алгоритм наивного байеса.
naive_bias <- function(x, y, tlyambda = NA, tmu = NA, ttsigma = NA, taprior = NA) {
  #x - объект для классификации
  #y - выборка
  #tmu - матожиданий предоставленные пользователем.
  #ttsigma - сигмы предоставленные пользователем 
  #taprior - априорные вероятности предоставленные пользователем
  
  mu <- NA #мат ожидания
  tsigma <- NA #сигмы
  aprior <- NA #априорные вероятности
  lyambda <- NA #переменные штрафа за ошибку
  # print("ALAHAMORA")
  lvls <- levels(iris$Species)
  
  #условия проверяющие дал ли пользователь какие-нибудь начальные данные
  #если не дал, то высчитать самостоятельно
  if(is.na(tmu) == FALSE) {
    mu <- tmu
  } else {
    mu <- calc_mu(y)
  }
  
  if(is.na(ttsigma) == FALSE) {
    tsigma <- ttsigma 
  } else {
    tsigma <- calc_sigma(y)
  }
  
  if(is.na(taprior) == FALSE) {
    aprior <- taprior
  } else {
    aprior <- calc_aprior_prob(y)
  }
  
  #использовать датафрейм мат ожиданий, чтобы посчитать колличество классов
  l <- dim(mu)[1]
  
  if (is.na(tlyambda) == FALSE) {
    lyambda <- tlyambda
  } else {
    lyambda <- seq(1, l)
  }
  
  
  maxv <- -1 #начальный максимальный результат
  best_class <- aprior[1,2] #счтитаем клссом по умолчанию первый класс
  
  
  
  for (i in 1:l) {
    tclass <- aprior[i,2]
    aprior_part <- log(lyambda[i] * aprior[i, 1])
    tight_part <- calc_tight(x, tclass, mu, tsigma)
    
    united_part <- aprior_part + tight_part
    
    if(united_part > maxv) {
      maxv <- united_part
      best_class <- tclass
    }
  }
  
  maxv = exp(maxv)
  
  best_class <- lvls[best_class]
  
  ans <- c(best_class, as.numeric(maxv))
  ans <- data.frame(ans[1], maxv)
  names(ans) <- c("class", "tig")
  
  return(ans)
}

make_map <- function(colors, classifier = naive_bias, k = NA, q = NA) {
  #
  
  # plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
  # points(iris[, 3:4], pch = 22,
  #        bg = colors[iris$Species],
  #        col 
  #        = colors[iris$Species], asp = 1)
  
  i = 0
  j = 0
  
  
  tmps <- data.frame()
  
  while (i <= 7) {
    while(j < 2.6) {
      p <- iris[1, 1:(dim(iris)[2] - 1)]
      p[3] = i
      p[4] = j
      
      t <- classifier(p[,3:4], iris[,3:5])
      
      p <- cbind(p, t[1], t[2])
      # points(p[,3], p[,4], pch = 22,
      #        # bg = colors[p[, 5]],
      #        col = colors[p$class], alpha = p[,6])
      j <- j + 1/10
      
      tmps <- rbind(tmps, p)
    }
    i <- i + 1/10
    j <- 0
  }
  
  
  # print(tmps)
  # contourplot(tig ~ Petal.Length * Petal.Width, data = tmps, region = TRUE)
  # 
  return(tmps)
}

make_statistic_map <- function(point, classifier = naive_bias, color) {
  #
  
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
  
  i = 0
  j = 0
  while (i <= 6.9) {
    while(j < 2.5) {
      p <- iris[1:4, 1:(dim(iris)[2] - 1)]
      p[1, 3] = i
      p[1, 4] = j
      
      p[2, 3] = i + 0.1
      p[2, 4] = j
      
      p[3, 3] = i + 0.1
      p[3, 4] = j + 0.1
      
      p[4, 3] = i
      p[4, 4] = j + 0.1
      
      tt <- data.frame()
      
      t <- classifier(p[1,3:4], iris[,3:5])
      tt <- rbind(tt, t)
      
      t <- classifier(p[2,3:4], iris[,3:5])
      tt <- rbind(tt, t)
      
      
      t <- classifier(p[3,3:4], iris[,3:5])
      tt <- rbind(tt, t)
      
      
      t <- classifier(p[4,3:4], iris[,3:5])
      tt <- rbind(tt, t)
      
      p <- cbind(p, tt)
      
      classes <- table(p$class)
      # points(p[,3], p[,4], pch = 21,
      #        # bg = colors[p[, 5]],
      #        col = colors[p$class], cex = as.numeric(p$tig) + 1)
      
      # print("kek")
      # print(levels(p$class))
      # print(classes)
      # print(names(which.max(classes)))
      
      nmn <- names(which.max(classes))
      # print(color[nmn])
      
      # print(which(p$class == nmn)[1])
      # 
      # nmn <- which(p$class == nmn)[1]
      # print(nmn)
      polygon(x = p$Petal.Length, y = p$Petal.Width, col = color[nmn])
      j <- j + 1/10
      
    }
    i <- i + 1/10
    j <- 0
  }
}


colors <- c("setosa" = "red", "versicolor" = "green3", 
            "virginica" = "blue", "na" = "yellow") 


colors2 <- c("1" = "red", "2" = "green3", 
            "3" = "blue", "na" = "yellow") 
make_map(colors)

aa <- make_map(colors)

make_statistic_map(aa, color = colors2)
aa$class <- factor(aa$class)
aa

contourplot(class ~ Petal.Length * Petal.Width, data = aa, region = TRUE)


x <- iris[1, ]
x[,3] = 3.1
x[,4] = 1.2
x
# plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col 
#      = colors[iris$Species], asp = 1)
# 
# plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
# points(iris[, 3:4], pch = 21, bg = colors[iris$Species], col 
#      = colors[iris$Species], asp = 1)

ans <- data.frame()
for(i in 1:dim(x)[1]) {
  ans <- rbind(ans, naive_bias(x[i, 3:4], iris[, 3:5]))
}

naive_bias(x[3:4], iris[, 3:5])

str(ans)

x <- cbind(x, ans)
x
plot(x[,3], x[,4], pch = 22, bg = colors[x[, 5]],
       col = colors[x[, 5]], cex = as.numeric(x[,6]) + 1)


points(x[,3], x[,4], pch = 22, bg = colors[x[, 5]],
     col = colors[x[, 5]], cex = x[,6] + 1)
