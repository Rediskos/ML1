

calc_prob_rasp <- function(x, mu, E) {
  #x - точка как вектор
  #mu - мат ожидание как вектор
  #E - ковариационная матрица
  
  mu <- as.numeric(mu)
  x <- as.numeric(x)
  t_mu <- t(mu)
  t_x <- t(x)
  solve_E <- solve(E)
  left_part <- (t_mu %*% solve_E %*% mu) / (-2)
  
  right_part <- t_x %*% solve_E %*% mu 
  
  ans <- left_part + right_part
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
    
    ans <- rbind.data.frame(ans, cbind.data.frame(mus_for_one, i))
  }
  names(ans) <- names(features)
  
  return(ans)
}

#возвращает ковариационную матрицу по признакам
#и мат ожиданию класса
calc_cov_matr <- function(y, tmu, tl) {
  #features - датафрейм признаков где последний столбец класс
  #tmu - мат ожидание, однострочная матрица
  
  l <- dim(y)[2] #количество разных признаков
  m <- dim(y)[1] #колличество признаков
  ans <- matrix(0, nrow = l - 1, ncol = l - 1) #матрица ответ
  
  for(j in levels(y[, l])) {
    same_class <- which(y[, l] == j) #найти индексы объектов одинакового класса
    yj <- y[same_class, ] #взять объекты одинакового класса из выборки
    tm = dim(yj)[1]
    
    mu_for_j <- which(tmu[, l] == j)#индекс мат ожидания класса j
    # E <- calc_cov_matr(yj, mu[mu_for_j, ], yl) #посчитать ковариационную матрицу
    
    
    
    num_mu <- as.numeric(tmu[mu_for_j , 1:l - 1])
    mu <- t(as.matrix(num_mu))
    
    for (i in 1:tm) {
      x_i <- as.matrix(yj[i, 1:l-1], nrow = 1)
      cov_mt_xi <- t(x_i - mu) %*% (x_i - mu)
      ans <- ans + cov_mt_xi / (m + 2)
    }
    
  }
  # ans <- ans / tl
  
  
  # ans <- ans / tl #поправка на смещённость
  
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
LDF <- function(x, y, tlyambda = NA, tmu = NA, taprior = NA) {
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
  
  tmp_class <- y[1, yc]
  all_tmp_class <- which(y[, yc] == tmp_class)
  E <- calc_cov_matr(y, mu, yl)#так как все ковариацинные матрицы равны
  
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
      # E <- calc_cov_matr(yj, mu[mu_for_j, ], yl) #посчитать ковариационную матрицу
      
      lyambda_j <- lyambda[j] #взять ошибку для класса j
      for_aprior <- which(aprior[, 2] == j) 
      aprior_j <- aprior[for_aprior, 1] # взять априорную вероятность для класса j
      tig <- calc_prob_rasp(x_i, mu = mu[mu_for_j, 1:yc-1], E = E) #посчитать плотность
      
      tmp_ans <-  log(lyambda_j * aprior_j) + tig #вероятный ответ
      
      #проверка лучше ли вероятный ответ чем предыдущий лучший
      if(tmp_ans > maxv) {
        maxv <- tmp_ans
        best_class <- j
      }
    }
    
    best_ans_for_xi <- c(best_class, as.numeric(maxv)) 
    ans <- rbind(ans, best_ans_for_xi)
  }
  
  name_class_col <- names(y)[yc]
  names(ans) <- c(name_class_col, "tig")
  return(ans)  
}

make_div_line3 <- function(x, lyambdas, tfrom = -4, tto = 4, step = 0.01,
                           taprior = NA) {
  #x - датафрейм точек уже классифицированых
  #lyambdas - вектор важностей классов
  
  xc <- dim(x)[2] #колличество столбцов выборкиS
  xl <- dim(x)[1]
  
  mu <- calc_mu(x)
  mu1 <- t(as.matrix(mu[1,1:2]))
  
  mu2 <- t(as.matrix(mu[2,1:2]))
  # z <- mu[, c(1:xc-1)]
  # tmp <- sapply(z, as.numeric)
  # z <- cbind.data.frame(tmp, mu[, xc])
  # mu[,] <- z[, c(1:xc-1)]
  # mu[, c(1:xc-1)] <- sapply(mu[,c(1:xc-1)], as.numeric)
  aprior <- aprior_prob(x)
  if(is.na(taprior) == FALSE) {
    aprior <- taprior
  }
  solvs_E <- data.frame()
  dets_E <- data.frame()
  
  
  point <- data.frame()
  
  l <- dim(x)[1]
  
  E <- calc_cov_matr(x, mu, l) #посчитать ковариационную матрицу
  
  s_E <- solve(E)
  
  A1 <- s_E %*% mu1
  A2 <- s_E %*% mu2
  A <- A1 - A2
  B1 <- log(lyambdas[1] * aprior[1,1]) - log(lyambdas[2] * aprior[2,1])
  B2 <- t(mu1) %*% s_E %*% mu1
  B3 <- t(mu2) %*% s_E %*% mu2
  B <-B1 + -B2 / 2 + B3 / 2
  
  return(c(A, B))
}

make_div_line2 <- function(x, lyambdas, tfrom = -4, tto = 4, step = 0.01) {
  #x - датафрейм точек уже классифицированых
  #lyambdas - вектор важностей классов
  
  xc <- dim(x)[2] #колличество столбцов выборкиS
  xl <- dim(x)[1]
  
  mu <- calc_mu(x)
  # z <- mu[, c(1:xc-1)]
  # tmp <- sapply(z, as.numeric)
  # z <- cbind.data.frame(tmp, mu[, xc])
  # mu[,] <- z[, c(1:xc-1)]
  # mu[, c(1:xc-1)] <- sapply(mu[,c(1:xc-1)], as.numeric)
  aprior <- aprior_prob(x)
  solvs_E <- data.frame()
  dets_E <- data.frame()
  
  
  point <- data.frame()
  
  l <- dim(x)[1]
  
  E <- calc_cov_matr(x, mu, l) #посчитать ковариационную матрицу
  solvs_E <- as.numeric(as.vector(solve(E))) #векторизовать ковариационную матрицу0
  
  
  #получить точки соответствующей разделяющей прямой
  #для двух классов
  for (i in seq(from = tfrom, to = tto, by = step)) {
    #коэфициент при х
    A <- solvs_E[1] * mu[1,1] + solvs_E[2]*mu[1,1] - solvs_E[1]*mu[2,1] -
      solvs_E[2]*mu[2,1]
    
    #коеф при y
    B <- solvs_E[3]*mu[1,2] + solvs_E[4]*mu[1,2] - solvs_E[3]*mu[2,2] -
      solvs_E[4]*mu[2,2]
    
    B <- B * i
    
    
    #коеф при -1/2
    C <-solvs_E[1]*(mu[1,1]^2) + solvs_E[4]*(mu[1,2]^2) +
      (solvs_E[3] + solvs_E[2])*mu[1,2]*mu[1,1]
    
    C <- -C / 2
    
    #коеф при 1/2
    D <-solvs_E[1]*(mu[2,1]^2) + solvs_E[4]*(mu[2,2]^2) +
    (solvs_E[3] + solvs_E[2])*mu[2,2]*mu[2,1]
    
    D <- D / 2
    
    #с логарифмами
    E <- -log(lyambdas[2]*aprior[2,1]) + log(lyambdas[1]*aprior[1,1])
    
    Const_part <- B + C + D + E
    
    if(abs(A) > 1e-6) {
      p <- -Const_part / A
    } else {
      p <- NA
    }
  
    #если точка была комплексной, то пропустить её
    if(is.na(p) == FALSE) {
      point <- rbind.data.frame(point, cbind.data.frame(p, i))
    }
  }
  return(point)
}
# tx <- iris
# ty <- c(1:3)
# aa$class <- as.factor(aa$class)
# tmap$class <- as.factor(tmap$class)
# tmap_div_line <- make_div_line(tmap[, 1:3], c(1:2))

# tmap_div_line <- make_div_line2(lft[, 1:3], c(1,1))

make_map <- function(y, params, classifier = LDF) {
  
  i = 0
  j = 0
  
  
  tmps <- data.frame()
  rem <- names(y)
  r2 <- NA
  while (i <= 7) {
    while(j < 2.6) {
      p <- y[1, 1:(dim(y)[2] - 1)]
      p[, params] <- c(i, j)
      
      ty <- cbind(y[, params], y[, dim(y)[2]])
      
      t <- classifier(p[,params], ty)
      r2 <- names(t[2])
      p <- cbind(p, t[1], t[2])
      # points(p[,3], p[,4], pch = 22,
      #        # bg = colors[p[, 5]],
      #        col = colors[p$class], alpha = p[,6])
      j <- j + 0.1
      
      tmps <- rbind(tmps, p)
    }
    i <- i + 0.1
    j <- 0
  }
  
  
  names(tmps) <- c(rem, r2)
  return(tmps)
}
# tmap <- make_map(lft, 1:2)
# tmap[1,]

draw_map <- function(xx, yy, zz, mm) {

  xc <- dim(xx)[2]
  class <- levels(xx[, xc])
  
  # ggplot(data = xx, aes(x = Petal.Length, y = Petal.Width)) +
  #   
  #   geom_point(shape = 22, aes(alpha = tig, fill = Species), 
  #              size = 4, stroke = 0)+ 
  #   scale_alpha_continuous(name = "Плотность") + 
  #   scale_fill_manual(values = c("red", "green", "blue"), name = "Классифицирован")+
  #   
  #   geom_point(data = yy, shape = 22, aes(col = Species), size = 2, stroke = 2) +
  #   scale_color_manual(values = c("red", "green", "blue"), name = "Выборка")+
  #   labs(title = "Карта классификации LDF алгоритма: ирисы Фишера") 
  
  # ggplot(data = xx, aes(x = Petal.Length, y = Petal.Width)) +
  #   geom_point(data = yy, shape = 22, aes(fill = Species), size = 2, stroke = 0) +
  #   scale_fill_manual(values = c("red", "green", "blue"), name = "Класс")+
  #   
  #   geom_point(shape = 22, aes(alpha = tig, fill = Species), 
  #              size = 4, stroke = 0)+ 
  #   scale_fill_manual(values = c("red", "green", "blue"), name = "Класс")+
  #   scale_alpha_continuous(name = "Плотность нормированная сигмоидой") + 
  #   # scale_color_manual(values = c("red", "green", "blue"), name = "Выборка")+
  #   
  #   labs(title = "Карта классификации LDF алгоритма: ирисы Фишера") 
  
  my.scales <- list(
    scale_x_continuous(name="Petal.Length", limits=c(0,7)),
    scale_y_continuous(name="Petal.Width", limits=c(0,2.6)),
    scale_fill_manual(values = c("red", "green", "blue"), name = "Класс"),
    scale_alpha_continuous(name = "Плотность"),
    scale_linetype(name="Разделяющая прямая")
  )
  
  ggplot(data = xx, aes(x = Petal.Length, y = Petal.Width)) +
    
    geom_point(shape = 22, aes(alpha = tig, fill = Species), 
               size = 5, stroke = 0)+ 
    geom_abline(data = zz, mapping = aes(slope = xx, intercept = yy), size = 2) +
    geom_abline(data = mm, mapping = aes(slope = xx, intercept = yy), size = 2) +
    # scale_alpha_continuous(name = "Плотность нормированная сигмоидой") + 
    geom_point(data = yy, shape = 22, aes(fill = Species), size = 2, stroke = 2) +
    
    # scale_linetype(name="s") +
    my.scales+
    
    # 
    # xlim(c(0,7)) +
    # ylim(c(0,2.6))+
  

    
    # scale_color_manual(values = c("red", "green", "blue"), name = "Выборка")+
    
    labs(title = "Карта классификации PlugIn алгоритма: ирисы Фишера") 
    
}

# 
# tmap$tig <- sigmoid(as.numeric(tmap$tig))
# tmap$tig <- as.numeric(tmap$tig)
# draw_map(tmap, lft)
# 
# tmap

sigmoid = function(x) {
  1 / (1 + exp(-x))
}

# 
# x <- iris[1,]
# 
# 
# ans <- LDF(x[3:4], iris[3:5])
# 
# ans


# aa <- make_
# str(aa)map()
#
# 
# contourplot(tig ~ Petal.Length * Petal.Width, data = aa, region = TRUE)
# 
# options(device = "windows")
# dev.cur()
# draw_map(aa, iris)
# z <- make_div_line3(not_verginia[3:5], c(1,1), taprior = rem_aprior)

zz <- data.frame(xx = z[1]/(-z[2]),yy = z[3]/(-z[2]))
zz


m <- make_div_line3(not_setosa[3:5], c(1,1), taprior = rem_aprior)
mm <- data.frame(xx = m[1]/(-m[2]), yy = m[3]/(-m[2]))

rem_aprior <- calc_aprior_prob(iris)
rem_aprior

aa <- make_map(not_verginia, 3:4)
aa[1,]
cc <- aa
cc[1,]
cc$tig <- as.numeric(aa$tig)
bb <- aa
bb[1,]
bb$tig <- as.numeric(aa$tig)
# bb$tig <- sigmoid(bb$tig)
# names(bb)
zz
mm
draw_map(bb, iris, zz, mm)
# levels(iris$Species)
not_verginia <- iris[iris$Species != "virginica",]

not_verginia$Species <- as.character(not_verginia$Species)
not_verginia$Species <- as.factor(not_verginia$Species)
not_setosa <- iris[iris$Species != "setosa", ]
not_setosa$Species <- as.character(not_setosa$Species)
not_setosa$Species <- as.factor(not_setosa$Species)
asd <- not_verginia[, 3:5]
asd
not_verginia_div_line <- make_div_line2(not_verginia[, 3:5], c(1,1), 0, 3, 0.01)
names(not_setosa_div_line)
not_setosa_div_line <- make_div_line2(not_setosa[, 3:5], c(1,1), 0, 3, 0.01)

lft

lft

c <- make_div_line3(lft, c(1,1))
cc <- data.frame(xx = c[1]/(-c[2]), yy = c[3]/(-c[2]))
cc
str(bb)
str(lft)
ggplot(data = lft, aes(x = first, y = second)) +
  geom_point(shape = 22) +
  # # scale_linetype(name="s") +
  geom_abline(data = cc, mapping = aes(slope = xx, intercept = yy))
  scale_x_continuous( limits=c(-10,10)) +
  scale_y_continuous(limits=c(-10,10))
