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

LOG_RES_MAKE_MAP <- function(w) {
  l <- dim(w)[1]
  wt <- as.matrix(w[l,1:3])
  
  t_wt <- t(wt)
  
  ans <- data.frame()
  
  for(i in 0:100) {
    for(j in 0:100) {
      x1 <- data.frame(
        shift = 1,
        xx = i/100,
        yy = j/100
      )
      
      x <- as.matrix(x1)
      
      
      
      for_simg <- wt %*% t(x) * -1 
      y_minus <- sigmoida(for_simg)
      
      for_simg <- wt %*% t(x) * 1
      y_plus <- sigmoid(for_simg)
      
      for_ans <- data.frame()
      
      if (y_minus > y_plus) {
        for_ans <- cbind.data.frame(
          x1, -1, y_minus
        )
      } else {
        
        for_ans <- cbind.data.frame(
          x1, 1, y_plus
        )
      }
      
      # print(for_ans)
      
      
      if(dim(ans)[1] > 0) {
        names(for_ans) <- names(ans)
        ans <- rbind.data.frame(ans, for_ans)
      } else {
        ans <- for_ans
      }
    }
  }
  
  names(ans) <- c("shift", "xx", "yy", "class", "prob")
  
  return(ans)
}

ADALINE_draw_line1 <- function(x, w1, w2, w3) {
  #x - data.frame - выборка - три столбца
  #w - data.frame - веса - три стоблца
  p <- ggplot(x, aes(x = x[,1], y = x[,2], fill = x[,3])) + 
    geom_point(size = 4, stroke = 1, shape = 21)
  
  l1 <- dim(w1)[1]
  l2 <- dim(w2)[1]
  l3 <- dim(w3)[1]
  
  
  slope_tmp <- -w1[l1, 2] / w1[l1, 3]
  intercept_tmp <- -w1[l1,1] / w1[l1, 3]
  p <- p + geom_abline(intercept = intercept_tmp,
                       slope = slope_tmp, colour = "blue", size = 3)
  
  slope_tmp <- -w2[l2, 2] / w2[l2, 3]
  intercept_tmp <- -w2[l2,1] / w2[l2, 3]
  p <- p + geom_abline(intercept = intercept_tmp,
                       slope = slope_tmp, colour = "red", size = 3)
  
  slope_tmp <- -w3[l3, 2] / w3[l3, 3]
  intercept_tmp <- -w3[l3,1] / w3[l3, 3]
  p <- p + geom_abline(intercept = intercept_tmp,
                       slope = slope_tmp, colour = "green", size = 3) + 
    scale_fill_manual(values = c("yellow", "red"), name = "Класс")
  p <- p + ylim(0,1) + xlim(0,1) + labs(title = "SGD, Логистическая регрессия")
  print(p)
}

# LOG_REG_SGD <- SGD(for_ADALINE,
#                 lyambda = 0.1, 
#                 los_func = LOG_REG_LOSS_FUNC,
#                 los_func_deriv = LOG_RES_LOSS_FUNC_DERIV, 
#                 return_all_weights =  TRUE,
#                 reg_tau = 0.5, steps = 200)

# ADALINE_draw_los_change_line(LOG_REG_SGD)

# norm_hebb <- for_ADALINE
# norm_hebb[1,]
# norm_hebb[, 1:2] <- normilize_X(norm_hebb[, 1:2])
# 
# ADALINE_draw_line1(norm_hebb, HEBB_SGD, ADALINE_SGD, LOG_REG_SGD)

map.dots <- LOG_RES_MAKE_MAP(LOG_REG_SGD)
