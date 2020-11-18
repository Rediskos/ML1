

#функция стохастического градиентного спуска
SGD <- function(xl, learn_temp, lyambda, los_func, los_func_deriv) {
  #xl - датафрейм векторов выборки
  #learn_temp - темп обучения
  #lyambda - параметр сглаживания
  #los_func - функция потерь
  #los_func_deriv - производная функции потерь
  
  l <- dim(xl)[1] #размер выборки
  x <- as.matrix(xl[, 1:l-1], ncol = l-1) #матрица параметров
  y <- as.matrix(xl[, l], ncol = 1) #матрица ответов
  m <- dim(x)[2] #сколько типов признаков 
  
  w <- runif(m, -1/(2*l), 1/(2*l)) #начальные веса
  
  Q <- 0 #функционал ошибки
  
  #начальная ошибка
  for(i in 1:l) {
    xi <- x[i,]
    yi <- y[i,]
    
    t_xi <- t(xi)
    
    for_los_func <- w %*% t_xi * yi
    
    los_func_res <- los_func(for_los_func)
    
    Q <- Q + los_func_res
  }
  
  Q_past <- Q + 10000#значение ошибки с предыдущего шага.
  
  while (abs(Q_past - Q) > 1e-4) {
    rand_indx <- sample(1:l, 1)
    xi <- x[rand_indx,]
    yi <- y[rand_indx,]
    
    t_xi <- t(xi)
    
    for_los_func <- w %*% t_xi * yi
    res_from_los <- los_func(for_los_func)
    
    res_from_los_deriv <- los_func_deriv(for_los_func)
    
    for_weight_step <- res_from_los  * xi * yi
    
    w <- w - learn_temp * for_weight_step
    
    Q_past <- Q
    Q <- (1 - lyambda) * Q + lyambda * res_from_los
  }
  
  
  return(w)
}

