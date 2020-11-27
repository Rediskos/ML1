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
    
    ans <- rbind.data.frame(ans, cbind.data.frame(mus_for_one, i))
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
  
  if(m > 1) {
    ans <- ans / (m - 1)
  }
  
  return(ans)
}


#Считает априорные вероятности классов
calc_aprior_prob <- function(features) {
  #features - data.frame с выборкой по которой классифицирутся
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

#решает квадратное уравнение параболы
solv_quad <- function(a, b, c, ind) {
  disc <- b ^ 2 - 4 * a * c
  
  #если корень комплексный, то пропускаем точку
  if(disc < 0) {
    return(NA)
  }
  
  x1 <- (-b - sqrt(disc)) / (2 * a)
  x2 <- (-b + sqrt(disc)) / (2 * a)
  
  x1 <- c(x1, ind)
  x2 <- c(x2, -ind)
  
  ans <- x1
  
  #если решения свопали, то надо оставить только одну точку
  if(x1 != x2) {
    ans <- rbind(ans, x2)
  }
  
  return(ans)
}

#решает просто уравнение от x^2
solv_just_square <- function(a, c, ind) {
  for_sqrt <- (-c) / a
  
  if(for_sqrt < 0) {
    return(NA)
  }
  
  t <- sqrt(for_sqrt)
  x1 <- t
  x2 <- -t
  
  x1 <- c(x1, ind)
  x2 <- c(x2, -ind)
  
  ans <- x1
  if(x1 != x2) {
    ans <- rbind(ans, x2)
  }
  
  return(ans)
}

#решить линейное уравнение
solv_linear <- function(b,c, ind) {
  ans <- -c/b
  
  ans <- c(ans, ind)
  
  return(ans)
}


make_div_line2 <- function(x, lyambdas, tfrom = -4, tto = 4, step = 0.01,
                           taprior = NA, tE = NA) {
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
  
  if(is.na(taprior) == FALSE) {
    aprior <- taprior
  }
  
  point <- data.frame()
  
  l <- dim(x)[1]
  
  for(j in levels(x[, xc])) {
    same_class <- which(x[, xc] == j) #найти индексы объектов одинакового класса
    xj <- x[same_class, ] #взять объекты одинакового класса из выборки
    
    mu_for_j <- which(mu[, xc] == j)#индекс мат ожидания класса j
    E <- calc_cov_matr(xj, mu[mu_for_j, ]) #посчитать ковариационную матрицу
    
    ttt <- as.numeric(as.vector(solve(E))) #векторизовать ковариационную матрицу0
    solvs_E <- rbind(solvs_E, ttt) #добавить ВКМ ко ДФ из всех ВКМ
    dets_E <- rbind.data.frame(dets_E, det(E)) #Запомнить определитель ВКМ
  }
  
  
  ind <- 0
  
  #получить точки соответствующей разделяющей прямой
  #для двух классов
  for (i in seq(from = tfrom, to = tto, by = step)) {
    #коэффициент при x^2
    A <- solvs_E[1, 1] - solvs_E[2, 1]
    
    #коэффициенты при x^1
      #коэфициент при y
    B1 <- solvs_E[1,3] - solvs_E[2,3] + solvs_E[1,2] - solvs_E[2,2]
      #свободные коефициенты
    B2 <- 2*mu[2,1]*solvs_E[2,1] -
      2*mu[1,1]*solvs_E[1,1] +
      solvs_E[2,3] * mu[2,2] -
      solvs_E[1,3] * mu[1,2] +
      solvs_E[2,2] * mu[2,2] -
      solvs_E[1,2] * mu[1,2]
      #окончательный коефициент при x^1
    B <- B1 * i + B2
    
    #считаем константную часть
      #коэфициент при y^2
    D1 <- solvs_E[1,4] - solvs_E[2,4]
    
    #коефициент при y^1
    D2 <- solvs_E[2,3] * mu[2,1] - solvs_E[1,3] * mu[1,1] +
      solvs_E[2,2]*mu[2,1] - solvs_E[1,2]*mu[1,1] +
      solvs_E[2,4] * mu[2,2] * 2 - solvs_E[1,4]*mu[1,2]*2
    #коэфициенты при квадратах мат ожиданий
    D3 <- solvs_E[1,1]*(mu[1,1]^2) - solvs_E[2,1]*(mu[2,1] ^ 2) +
      solvs_E[1,4]*(mu[1,2]^2) - solvs_E[2,4]*(mu[2,2]^2)
    
    #коефициенты при переменожениях мат ожиданий
    D4 <- (solvs_E[1,3] + solvs_E[1,2])*mu[1,2]*mu[1,1] -
      (solvs_E[2,3] + solvs_E[2,2])*mu[2,2]*mu[2,1]
    
    #числитель логарифмической части
    D5 <- lyambdas[1] * aprior[1,1] * sqrt((2*pi)^2 * dets_E[2,1])
    
    #знаменатель логарифмической части
    D6 <- lyambdas[2] * aprior[2,1] * sqrt((2 * pi)^2 * dets_E[1,1])
    
    #Константная часть окончательно
    D <- D1 * i^2 + D2 * i + D3 + D4 - 2 * log(D5 / D6)
    
    #вектор ЫКСОВ, тк ответов может быть больше одного
    tmp <- vector()
    
    # if(A < 0){
    #   A <- -A
    #   B <- -B
    #   C <- -D
    # }
    
    #если коефициент при х^2 и x^1 не нулевой
    #то это квадратное уравнение
    if (A != 0 && B != 0) {
      tmp <- solv_quad(A, B, D, ind)
      # tmp <- NA
    } else
      #если коефициет при x^2 != 0 а при x^1 == 0
      #то это тоже квадратное уравнение, но решается иначе.
      if (A != 0 && B == 0) {
        tmp <- solv_just_square(A, D, ind)
        # tmp <- NA
      } else
        #если коефициет при x^2 == 0 а при x^1 != 0
        #то это линейное уравнение.
        if (A == 0 && B != 0) {
          tmp <- solv_linear(B, C, ind)
        }
    
    #если точка была комплексной, то пропустить её
    if(is.na(tmp) == FALSE) {
      point <- rbind.data.frame(point, cbind.data.frame(tmp, i))
    }
    
    ind <- ind + 1
  }
  return(point)
}
# tx <- iris
# ty <- c(1:3)
# aa$class <- as.factor(aa$class)
# tmap$class <- as.factor(tmap$class)
# tmap_div_line <- make_div_line(tmap[, 1:3], c(1:2))

tmap_div_line <- make_div_line2(lft[, 1:3], c(1,1))

tmap_div_line[, 2:3] <- tmap_div_line[, 3:2]
tmap_div_line[1,]

order(tmap_div_line[,3])
tord<- tmap_div_line[order(tmap_div_line[,3]), ]
tord

# 
# for_cc <- which(aa$class != "versicolor")
# 
# cc <- aa[for_cc, 1:5]
# cc
# names(cc)[5] <- "Species"
# 
# cc$Species <- as.factor(as.vector(cc$Species))
# str(cc)
# div_line <- make_div_line(cc[, 3:5], ty)
# div_line
# colors <- c("setosa" ="red", "virginica" = "green","versicolor" = "blue")
# 
# plot(aa$Petal.Length, aa$Petal.Width, col = colors[aa$class])
# points(div_line$tmp, div_line$i)

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
      j <- j + 0.05
      
      tmps <- rbind(tmps, p)
    }
    i <- i + 0.05
    j <- 0
  }
  
  
  names(tmps) <- c(rem, r2)
  
  return(tmps)
}

draw_map <- function(xx, yy, div_line1, div_line2, div_line3) {
  
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
    # scale_color_manual(values = c("red", "green", "blue"), name = "Выборка")++
    geom_path(data = div_line1, aes(x = one, y = thri)) +
    geom_path(data = div_line2, aes(x = one, y = thri)) +
    geom_path(data = div_line3, aes(x = one, y = thri)) +
    xlim(0, 7) +
    ylim(0, 2.6) +
    labs(title = "Карта классификации PlugIn алгоритма: ирисы Фишера") 
  }

not_versicolor$Species <- as.character(not_versicolor$Species)
not_versicolor$Species <- as.factor(not_versicolor$Species)
k
k <- make_div_line2(not_versicolor[,3:5], c(1,1), step = 0.001)
z <- make_div_line2(not_setosa[,3:5], c(1,1), tfrom = 0, tto = 2, step = 0.001)
m <- make_div_line2(not_verginia[,3:5], c(1,1),tfrom = 0, tto = 1.5,  step = 0.0001)
names(k) <- stdmemes
names(z) <- stdmemes
names(m) <- stdmemes

kk <- k[order(k$two),]
zz <- z[order(z$two),]
mm <- m[order(m$two),]

kk <- rbind.data.frame(kk, kk[1,])
zz <- rbind.data.frame(zz, zz[1,])
mm <- rbind.data.frame(mm, mm[1,])



draw_map(bb, iris, kk, zz, mm)

# x <- iris[1,]
# 
# ans <- PlugIn(x[3:4], iris[3:5])
# 
# ans
# 
# 
# aa <- make_map()
# str(aa)
# iris
# aa[which(aa$Species == "virginica"), ]
# 
# contourplot(tig ~ Petal.Length * Petal.Width, data = aa, region = TRUE)
# 
# 
# bb <- aa
# bb$tig <- sigmoid(as.numeric(aa$tig))
# names(bb)
# draw_map(bb, iris)


aa <- make_map(iris, 3:4)
aa[1,]
bb <- aa
bb[1,]
bb$tig <- as.numeric(aa$tig)
# bb$tig <- sigmoid(bb$tig)
# names(bb)

tmap_div_line <- make_div_line2(iri)

draw_map(bb, iris, ntsdl, ntvdl)
# levels(iris$Species)
not_verginia <- iris[iris$Species != "virginica",]

not_verginia$Species <- as.character(not_verginia$Species)
not_verginia$Species <- as.factor(not_verginia$Species)
not_setosa <- iris[iris$Species != "setosa", ]
not_setosa$Species <- as.character(not_setosa$Species)
not_setosa$Species <- as.factor(not_setosa$Species)
asd <- not_verginia[, 3:5]
asd
not_verginia_div_line <- make_div_line2(not_verginia[, 3:5], c(1,1), 0, 3, 0.001)
names(not_setosa_div_line)
not_setosa_div_line <- make_div_line2(not_setosa[, 3:5], c(1,1), 0, 3, 0.001)

not_versicolor <- iris[iris$Species != "versicolor",]

str(iris)

not_setosa_div_line <- not_setosa_div_line[order(not_setosa_div_line[,2]),]
not_verginia_div_line <- not_verginia_div_line[order(not_verginia_div_line[,2]),]

ntsdl <- rbind.data.frame(not_setosa_div_line, not_setosa_div_line[1,])
ntvdl <- rbind.data.frame(not_verginia_div_line, not_verginia_div_line[1,])

names(not_setosa_div_line) <- stdmemes
names(not_verginia_div_line) <- stdmemes


z <- rbind.data.frame(not_setosa_div_line, not_setosa_div_line[1,])

z
zz <- rbind.data.frame(not_verginia_div_line, not_verginia_div_line)
not_setosa_div_line[1,]

z <- make_div_line3(not_verginia[3:5], c(1,1))
z

zz <- data.frame(xx = z[1]/(-z[2]),yy = z[3]/(-z[2]))
zz


m <- make_div_line3(not_setosa[3:5], c(1,1))
mm <- data.frame(xx = m[1]/(-m[2]), yy = m[3]/(-m[2]))
not_versicolor
seq(from = 1, by = 3, length.out = dim(ADALINE_SGD) / 3)

as.numeric(levels(for_ADALINE$thri)[for_ADALINE$thri])
