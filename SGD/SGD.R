

#функция стохастического градиентного спуска
SGD <- function(xl, learn_temp, lyambda, los_func, los_func_deriv,
                return_all_weights = FALSE, reg_tau = 0.5, steps = 1000) {
  #xl - датафрейм векторов выборки
  #learn_temp - темп обучения
  #lyambda - параметр сглаживания
  #los_func - функция потерь
  #los_func_deriv - производная функции потерь
  
  l <- dim(xl)[1] #размер выборки
  ml <- dim(xl)[2] #сколько типов признаков
  nms <- names(xl[, 1:ml-1])
  x <- as.matrix(xl[, 1:ml-1], ncol = ml-1) #матрица параметров
  y <- as.matrix(xl[, ml], ncol = 1) #матрица ответов
  
  m <- dim(x)[2]
  w <- runif(m, -1/(2*l), 1/(2*l)) #начальные веса
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
  
  wt <- c(w, Q)
  
  response <- wt
  
  while (steps > 0) {
    
    
    rand_indx <- sample(1:l, 1)
    xi <- x[rand_indx,]
    yi <- y[rand_indx,]
    
    res_from_los <- los_func(w, xi, yi) + reg_tau / 2 * t(w) %*% w
    
    res_from_los_deriv <- los_func_deriv(w, xi, yi)
    
    for_weight_step <- res_from_los_deriv  * xi * yi
    
    w <- w*(1 - learn_temp*reg_tau) - learn_temp * for_weight_step
    
    
    Q_past <- Q
    Q <- (1 - lyambda) * Q + lyambda * res_from_los
    
    wt <- c(w, Q)
    
    if(return_all_weights == TRUE) {
      response <- rbind.data.frame(response, wt)
    } else {
      response <- wt
    }
    
    
    if(learn_temp > 1e-4) {
      learn_temp <- learn_temp * 0.5
    }
    
    tabs <- abs(Q_past - Q)
    steps <- steps - 1
  }
  
  names(response) <- c(nms, "loss")
  response <- response[order(-response$loss), ]
  return(response)
}

