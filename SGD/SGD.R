
#функция нормализации
normilize_X <- function(x) {
  m <- dim(x)[2]
  
  if(is.null(m)) {
    m <- dim(x)[1]
    minv <- min(x)
    maxv <- max(x)
    
    if(maxv - minv < 1e-8) {
      x <- rep(0, m)
    } else {
      
      x <- (x - minv) / (maxv - minv)
    }
    
  } else {
    for(i in 1:m) {
      minv <- min(x[,i])
      maxv <- max(x[,i])
      if(maxv - minv < 1e-8) {
        x[,i] <- 0
      } else {
        
        x[,i] <- (x[,i] - minv) / (maxv - minv) 
      }
    }
  }
  
  # x[,1] <- x[,1] - 0.5
  
  return(x)
}

norm_vec <- function(x) sqrt(sum(x^2))

def_leanr_rate_calc <- function(x, t) {
  response <- 1/t
  return(response)
}



#функция стохастического градиентного спуска
SGD <- function(xl, learn_temp_func = def_leanr_rate_calc,
                lyambda, los_func, los_func_deriv,
                return_all_weights = FALSE, reg_tau = 0.5, steps = 1000,
                normIt = TRUE) {
  #xl - датафрейм векторов выборки
  #learn_temp - темп обучения
  #lyambda - параметр сглаживания
  #los_func - функция потерь
  #los_func_deriv - производная функции потерь
  #return_all_weights - TRUE, если требуется вернуть не просто веса, а историю изменения
  #reg_tau - параметри регуляризации
  #steps - максимальное колличество шагов алгоритма
  #normIt - TRUE, если требуется нормализовать признаки
  
  l <- dim(xl)[1] #размер выборки
  
  
  
  ml <- dim(xl)[2] #сколько типов признаков
  # nms <- c("shift", names( xl[, 1:ml-1]))
  nms <- names( xl[, 1:ml-1])
  
  x <- as.matrix(xl[, 1:ml-1], ncol = ml-1) #матрица параметров
  y <- xl[, ml] #матрица ответов
  
  ty <- levels(y)[y]
  ty <- as.numeric(ty)
  
  y <- matrix(ty, ncol = 1)
  
  
  
  if(normIt == TRUE) {
    x <- normilize_X(x)
  }
  
  global_x <- x
  
  add_x <- matrix(rep(1, l), ncol = 1)#столбец для смещений
  
  
  xplot <- cbind.data.frame(x, as.factor(y))
  xplot <- as.data.frame(xplot)
  x <- cbind(add_x, x)
  
  
  m <- dim(x)[2]
  w <- runif(m , -1/(2*l), 1/(2*l)) #начальные веса
  names(w) <- nms
  
  Q <- 0 #функционал ошибки
  
  #начальная ошибка
  for(i in 1:l) {
    xi <- x[i,]
    yi <- y[i,]
    
    los_func_res <- los_func(w, xi, yi)
    
    Q <- Q + los_func_res
  }
  
  Q_past <- Q + 10000#значение ошибки с предыдущего шага.
  
  wt <- c(w, Q)#веса вместе с ошибкой на них
  
  response <- wt #возвращаемый из функции ответ
  tabs <- 1
  
  
  
  
  step = 1
  for_learn_temp = 1
  
  yi = 0
  
  colorr <- c( "1" = "red", "-1" = "yellow")
  
  while ((Q > 0.1 || tabs > 0.1) && step <= steps) {
    
    kkk <- 30
    rand_indx <- sample(1:l, 1) #случайный индекс
    txi <- x[rand_indx,]#случайные признаки
    tyi <- y[rand_indx,]#случайный ответ
    
    while(tyi == yi && kkk > 0) {
      
      rand_indx <- sample(1:l, 1) #случайный индекс
      txi <- x[rand_indx,]#случайные признаки
      tyi <- y[rand_indx,]#случайный ответ
      
      kkk <- kkk - 1
    }
    
    xi <- txi
    yi <- tyi
    
    
    learn_temp <- learn_temp_func(xi, for_learn_temp)#высчитать темп обучения
    
    res_from_los_deriv <- los_func_deriv(w, xi, yi)
    
    for_weight_step <- res_from_los_deriv
    
    
    
    res_from_los <- los_func(w, xi, yi) + reg_tau / 2 * (norm_vec(w) ^ 2)
    
    Q_past <- Q
    Q <- (1 - lyambda) * Q + lyambda * res_from_los
    
    
    wt <- c(w, Q)
    # pathh = "E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/ADALINE/aaa"
    # pathh <- paste(pathh, step, sep = "/")
    # pathh <- paste(pathh, "pdf", sep = ".")
    # pdf(file = pathh)
    # plot(xplot[, 1:2], pch = 21, xlim = c(0, 1), ylim = c(0,1),
    #      bg = colorr[as.numeric(xplot$`as.factor(y)`)])
    # points(xi[2], xi[3], pch = 22, bg = "blue", cex = 3)
    # abline(a = -w[1]/w[3], b = -w[2]/w[3])
    
    yit <- t(w) %*% xi
    
    if(return_all_weights == TRUE) {
      response <- rbind.data.frame(response, wt)
    } else {
      response <- wt
    }
    
    
    tabs <- abs(Q_past - Q)
    
    if(res_from_los - reg_tau / 2 * (norm_vec(w) ^ 2) > 1 && for_learn_temp > 1) {
      for_learn_temp <- for_learn_temp - 1
      w <- w*(1 - learn_temp*reg_tau) - learn_temp * for_weight_step
    } else {
      for_learn_temp <- for_learn_temp + 1
    } 
    
    # abline(a = -w[1]/w[3], b = -w[2]/w[3], col = "green")
    
    # dev.off()
    step <- step + 1
  }
  
  names(response) <- c(nms, "loss")
  return(response)
}

