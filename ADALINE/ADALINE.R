source("E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/SGD/SGD.R")
library(ggplot2)
norm_vec <- function(x) sqrt(sum(x^2))

ADALINE_learn_temp_calc <- function(x, t) {
  t_x <- norm_vec(x)
  response <-  t_x ^ 2
  return(response)
}

ADALINE_loss_func <- function(w, x, y) {
  t_x <- t(x)
  t_w <- t(w)
  for_ans <- t_w %*% x * y 
  ans <- (for_ans - 1)^2
  return(ans)
}

ADALINE_loss_func_deriv <- function(w, x, y) {
  t_x <- t(x)
  t_w <- t(w)
  for_ans <- t_w %*% x * y - 1
  ans <- 2 * for_ans * y * x
  return(ans)
}

ADALINE_draw_line <- function(x, w) {
  #x - data.frame - выборка - три столбца
  #w - data.frame - веса - три стоблца
  p <- ggplot(x, aes(x = x[,1], y = x[,2], fill = x[,3])) + 
    geom_point(size = 4, stroke = 1, shape = 21)
  
  l <- dim(w)[1]
  slope_tmp <- 0
  intercept_tmp <- 0
  for(i in 1:l) {
    slope_tmp <- -w[i, 2] / w[i, 3]
    intercept_tmp <- -w[i,1] / w[i, 3]
    p <- p + geom_abline(intercept = intercept_tmp, slope = slope_tmp, size = 1)
  }
  
  
  p <- p + geom_abline(intercept = intercept_tmp,
                       slope = slope_tmp, colour = "blue", size = 3) +
    scale_fill_manual(values = c("yellow", "red"), name = "Класс")
  p <- p + ylim(0,1) + xlim(0,1) + labs(title = "SGD, Логическая Регрессия")
  print(p)
}

ADALINE_draw_los_change_line <- function(w) {
  m <- dim(w)[1]
  
  tw <- w
  
  tw[,1] <- seq(from = 1, by = 1, length.out = m)
  
  ggplot(tw, aes(x = tw[,1], y = tw[,4])) + geom_path() +
    labs(x = "SGD шаг", y = "Эмпирический риск", 
         title = "SGD, функция потерь используя Логическую Регрессию", 
         subtitle = "График изменения эмпирического риска во время обучения")
}
ADALINE_draw_line(ttmp,ADALINE_SGD)
ADALINE_draw_line(ttmp,ADALINE_SGD[ptr,])

ADALINE_draw_los_change_line(ADALINE_SGD)

normilized_for_Adaline <- normilize_X()
ttmp <- for_ADALINE
ttmp$thri <- as.factor(ttmp$thri)

ADALINE_draw_line(ttmp,ADALINE_SGD)

fx <- runif(500,-1,-0.1)
fy <- runif(500,0,3)

sx <- runif(500, 0.1,1)
sy <- runif(500, 0,3)

fclass <- cbind.data.frame(fx, fy, 1)
sclass <- cbind.data.frame(sx, sy, -1)

tip_name <- c("one", "two", "thri")

names(fclass) <- tip_name
names(sclass) <- tip_name

for_ADALINE <- rbind.data.frame(fclass, sclass)
for_ADALINE$thri <- as.factor(for_ADALINE$thri)

for_ADALINE[1,]

ggplot(not_verginia, aes(x = Petal.Length, y = Petal.Width, colour = thri)) +
  geom_point() +
  scale_color_manual(values = c("red", "green"), name = "Класс")

ADALINE_SGD <- SGD(for_ADALINE,
                   lyambda = 0.1,
                   los_func = ADALINE_loss_func,
                   los_func_deriv = ADALINE_loss_func_deriv, 
                   return_all_weights =  TRUE,
                   reg_tau = 0.5, steps = 200)

ADALINE_draw_line(ttmp,ADALINE_SGD)

global_x <- 0

ADALINE_SGD
ADALINE_SGD <- ADALINE_SGD[order(-ADALINE_SGD$loss), ]
ptr <- 
  seq(from = 1, by = 2, length.out = dim(ADALINE_SGD)[1] / 3)
ptr
ADALINE_SGD[ptr,]
ADALINE_draw_line(norm_hebb,ADALINE_SGD)
