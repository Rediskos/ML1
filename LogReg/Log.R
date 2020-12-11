source("E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/SGD/SGD.R")
library(ggplot2)

sigmoida <- function(x) {
  return(1 / (1 + exp(-x)))
}

LOG_REG_LOSS_FUNC <- function(w, x, y) {
  t_w <- t(w)
  span <- t_w %*% x * y
  exp_for_log <- exp(-span)
  ans <- log2(1 + exp_for_log)
  return(ans)
}

LOG_RES_LOSS_FUNC_DERIV <- function(w, x, y) {
  t_w <- t(w)
  span <- t_w %*% x * y
  sigm_resp <- sigmoida(-span)
  response <- x * y * sigm_resp
  return(-response)
}

LOG_REG_SGD <- SGD(for_ADALINE,
                lyambda = 0.1, 
                los_func = LOG_REG_LOSS_FUNC,
                los_func_deriv = LOG_RES_LOSS_FUNC_DERIV, 
                return_all_weights =  TRUE,
                reg_tau = 0.5, steps = 200)

ADALINE_draw_los_change_line(LOG_REG_SGD)

norm_hebb <- for_ADALINE
norm_hebb[1,]
norm_hebb[, 1:2] <- normilize_X(norm_hebb[, 1:2])

ADALINE_draw_line(norm_hebb, LOG_REG_SGD)
