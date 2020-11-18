

#функция стозастического градиента
SDG <- function(xl, learn_temp, lyambda, los_func, los_func_deriv) {
  #xl - датафрейм векторов выборки
  #learn_temp - темп обучения
  #lyambda - параметр сглаживания
  #los_func - функция потерь
  #los_func_deriv - производная функции потерь
  
  l <- dim(xl)[1] #размер выборки
  x <- xl[, 1:l-1] #датафрейм параметров
  y <- xl[, l] #датафрейм ответов
  m <- dim(x)[2] #сколько типов признаков 
  
  w <- runif(m, -1/(2*l), 1/(2*l)) #начальные веса
  
  Q <- 0 #функционал ошибки
  
  #начальная ошибка
  for(i in 1:l) {
    xi <- x[i,]
    yi <- y[i,]
    
    for_los_func <- w %*% t(xi) * yi
    
    los_func_res <- los_func(for_los_func)
    
    Q <- Q + los_func_res
  }
  
  Q_past <- Q + 10000
  
  while (abs(Q_past - Q) > 1e-4) {
    rand_indx <- sample(1:l, 1)
    xi <- x[rand_indx,]
    yi <- x[rand_indx,]
    
    for_los_func <- w %*% t(xi) * yi
    res_from_los <- los_func(for_los_func)
    
    res_from_los_deriv <- los_func_deriv(for_los_func)
    
    for_weight_step <- res_from_los  * xi * yi
    
    w <- w - learn_temp * for_weight_step
    
    Q_past <- Q
    Q <- (1 - lyambda) * Q + lyambda * res_from_los
  }
  
}
