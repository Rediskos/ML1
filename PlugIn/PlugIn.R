#посчитать плотность в точке
#возвращает double
calc_prob_rasp <- function(x, mu, E) {
  #x - точка как вектор
  #mu - мат ожидание как вектор
  #E - ковариационная матрица
  
  mu <- as.numeric(mu)
  x_mu <- as.matrix(x - mu)
  t_x_mu <- t(x_mu)
  solve_E <- solve(E)
  for_exp <- (x_mu %*% solve_E %*% t_x_mu) / (-2)
  nominator <- exp(for_exp)
  
  for_sqrt <- (2 * pi) ^ dim(E)[1] * det(E)
  denominator <- sqrt(for_sqrt)
  
  ans <- nominator / denominator
  return(ans)
}

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

#возвращает ковариационную матрицу по признакам
#и мат ожиданию класса
calc_cov_matr <- function(features, mu) {
  #features - датафрейм признаков где последний столбец класс
  #mu - мат ожидание, однострочная матрица
  
  l <- dim(features)[2] #количество разных признаков
  m <- dim(features)[1] #колличество признаков
  
  num_mu <- as.numeric(mu[1 , 1:l - 1])
  mu <- t(as.matrix(num_mu))
  ans <- matrix(0, nrow = l - 1, ncol = l - 1) #матрица ответ
  
  for (i in 1:m) {
    x_i <- as.matrix(features[i, 1:l-1], nrow = 1)
    cov_mt_xi <- t(x_i - mu) %*% (x_i - mu)
    ans <- ans + cov_mt_xi
  }
  ans <- ans / (m - 1)
  
  return(ans)
}

calc_aprior_prob <- function(features) {
  m <- dim(features)[1]
  l <- dim(features)[2]
  
  tb <- table(features[,l]) / m
  
  tb <- data.frame(tb)
  ans <- tb[, 2:1]
  names(ans) <- c("aprior", names(features)[l])
  return(ans)
}

#основная функция PlugIn алгоритма
#возвращает датафрейм ответов: класс ~ его плотность для объекта
PlugIn <- function(x, y, tlyambda = NA, tmu = NA, taprior = NA) {
  #x - классифицируемые объекты
  #y - классифицирующая выборка
  
  
  xl <- dim(x)[1] #колличество классифицирующихся объектов
  yl <- dim(y)[1] #колличество объектов выборки
  yc <- dim(y)[2] #колличество столбцов выборкиS
  
  mu <- NA #мат ожидания
  aprior <- NA #априорные вероятности
  lyambda <- NA #переменные штрафа за ошибку
  
  #условия проверяющие дал ли пользователь какие-нибудь начальные данные
  #если не дал, то высчитать самостоятельно
  if(is.na(tmu) == FALSE) {
    mu <- tmu
  } else {
    mu <- calc_mu(y)
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
    lyambda <- rep(1, l)
  }
  
  names(lyambda) <- levels(y[, yc])
  
  
  ans <- data.frame() #
  
  #цикл классификации
  for (i in 1:xl) {
    x_i <- x[i, ] #взять i-тый елемент для классификации
    
    maxv <- -1e6 #максимальная плонтость
    best_class <- y[1, yc] #класс с максимальной плотностью
    
    #вложенный цикл классифицирующий один объект
    #идёт по классам
    for(j in levels(y[, yc])) {
      same_class <- which(y[, yc] == j) #найти индексы объектов одинакового класса
      yj <- y[same_class, ] #взять объекты одинакового класса из выборки
      
      mu_for_j <- which(mu[, yc] == j)#индекс мат ожидания класса j
      E <- calc_cov_matr(yj, mu[mu_for_j, ]) #посчитать ковариационную матрицу
      
      lyambda_j <- lyambda[j] #взять ошибку для класса j
      for_aprior <- which(aprior[, 2] == j) 
      aprior_j <- aprior[for_aprior, 1] # взять априорную вероятность для класса j
      tig <- calc_prob_rasp(x_i, mu = mu[mu_for_j, 1:yc-1], E = E) #посчитать плотность
      
      tmp_ans <- lyambda_j * aprior_j * tig #вероятный ответ
      
      #проверка лучше ли вероятный ответ чем предыдущий лучший
      if(tmp_ans > maxv) {
        maxv <- tmp_ans
        best_class <- j
      }
    }
    
    best_ans_for_xi <- c(best_class, maxv) 
    ans <- rbind(ans, best_ans_for_xi)
  }
  
  name_class_col <- names(y)[yc]
  names(ans) <- c(name_class_col, "tig")
  return(ans)  
}


make_map <- function(y, params, classifier = PlugIn) {
  # 
  # i = 0
  # j = 0
  # 
  # 
  # tmps <- data.frame()
  # 
  # while (i <= 7) {
  #   while(j < 2.6) {
  #     p <- iris[1, 1:(dim(iris)[2] - 1)]
  #     p[3] = i
  #     p[4] = j
  #     
  #     t <- classifier(p[,3:4], iris[,3:5])
  #     
  #     p <- cbind(p, t[1], t[2])
  #     # points(p[,3], p[,4], pch = 22,
  #     #        # bg = colors[p[, 5]],
  #     #        col = colors[p$class], alpha = p[,6])
  #     j <- j + 1/10
  #     
  #     tmps <- rbind(tmps, p)
  #   }
  #   i <- i + 1/10
  #   j <- 0
  # }
  
  i = 0
  j = 0
  
  
  tmps <- data.frame()
  rem <- names(y)
  r2 <- NA
  while (i <= 1) {
    while(j < 1) {
      p <- y[1, 1:(dim(y)[2] - 1)]
      p[, params] <- c(i, j)
      
      ty <- cbind(y[, params], y[, dim(y)[2]])
      
      t <- classifier(p[,params], ty)
      r2 <- names(t[2])
      p <- cbind(p, t[1], t[2])
      # points(p[,3], p[,4], pch = 22,
      #        # bg = colors[p[, 5]],
      #        col = colors[p$class], alpha = p[,6])
      j <- j + 0.05
      
      tmps <- rbind(tmps, p)
    }
    i <- i + 0.05
    j <- 0
  }
  
  
  names(tmps) <- c(rem, r2)
  
  return(tmps)
}

draw_map <- function(xx, yy) {
  
  xc <- dim(xx)[2]
  clas <- levels(xx[, xc])
  
  # ggplot(data = xx, aes(x = Petal.Length, y = Petal.Width)) +
  #   
  #   geom_point(data = yy, shape = 22, aes(fill = Species), size = 2, stroke = 2) +
  #   scale_fill_manual(values = c("red", "green", "blue"),
  #                     name = "Выборка")+
  #   
  #   geom_point(shape = 22, aes(alpha = tig, col = class), 
  #              size = 2, stroke = 2)+ 
  #   scale_color_manual(values = c("red", "green", "blue"), name = "Классифицирован")+
  #   scale_alpha_continuous(name = "Плотность") + 
  #   labs(title = "Карта классификации PlugIn алгоритма: ирисы Фишера")

  ggplot(data = xx, aes(x = Petal.Length, y = Petal.Width)) +
    geom_point(data = yy, shape = 22, aes(fill = Species), size = 2, stroke = 0) +
    scale_fill_manual(values = c("red", "green", "blue"), name = "Класс")+
    
    geom_point(shape = 22, aes(alpha = tig, fill = Species), 
               size = 4, stroke = 0)+ 
    scale_fill_manual(values = c("red", "green", "blue"), name = "Класс")+
    scale_alpha_continuous(name = "Плотность нормированная сигмоидой") + 
    # scale_color_manual(values = c("red", "green", "blue"), name = "Выборка")+
    
    labs(title = "Карта классификации PlugIn алгоритма: ирисы Фишера") 
  }


x <- iris[1,]

ans <- PlugIn(x[3:4], iris[3:5])

ans


aa <- make_map()
str(aa)
iris
aa[which(aa$Species == "virginica"), ]

contourplot(tig ~ Petal.Length * Petal.Width, data = aa, region = TRUE)


bb <- aa
bb$tig <- sigmoid(as.numeric(aa$tig))
draw_map(bb, iris)
