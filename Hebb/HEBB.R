source("E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/SGD/SGD.R")
library(ggplot2)

#Кусочно-линейная функция потерь
HEBB_LOSS_FUNC <- function(w, x, y) {
  t_x <- t(x)
  t_w <- t(w)
  for_ans <- t_w %*% x * y 
  ans <- for_ans
  if(ans > 0) {
    ans <- 0
  } else {
    ans <- -ans
  }
  return(ans)
}

#Правило хэбба(производная функции потерь)
HEBB_RULE <- function(w, x, y) {
  t_w <- t(w)
  for_ans <- x * y
  for_span <- t_w %*% x
  span <- for_span * y
  
  #Если отступ меньше нуля, то изменить веса
  if(span < 0) {
    #умножить ответ на -1, чтобы шаг градиентного спуска
    #соответствовал правилу Хэбба
    ans <- -for_ans
    return(ans) 
  } else {
    return(0)
  }
}



HEBB_SGD <- SGD(for_ADALINE,
                     lyambda = 0.1, 
                   los_func = HEBB_LOSS_FUNC,
                    los_func_deriv = HEBB_RULE, 
                   return_all_weights =  TRUE,
                   reg_tau = 0.5, steps = 200)

ADALINE_draw_los_change_line(HEBB_SGD)

norm_hebb <- for_ADALINE
norm_hebb[1,]
norm_hebb[, 1:2] <- normilize_X(norm_hebb[, 1:2])

ADALINE_draw_line(norm_hebb, HEBB_SGD)
