source("E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/SGD/SGD.R")
library(ggplot2)

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
  for_ans <- t_w %*% x * y^2 - y
  ans <- 2 * for_ans * x
  return(ans)
}

ADALINE_draw_line <- function(x, w) {
  p <- ggplot(x, aes(x = one, y = two, colour = thri)) + geom_point(size = 4) +
    scale_color_manual(values = c("red", "green"), name = "Класс")
  
  l <- dim(w)[1]
  tmp <- 0
  for(i in 1:l) {
    tmp <- -w[i, 2] / w[i, 1]
    p <- p + geom_abline(intercept = 0, slope = tmp)
  }
  
  p <- p + geom_abline(intercept = 0, slope = tmp, colour = "red", size = 2)
  p <- p + ylim(-2,4) + labs(title = "ADALINE")
  print(p)
}

ttmp <- for_ADALINE
ttmp$thri <- as.factor(ttmp$thri)

ADALINE_draw_line(ttmp,ADALINE_SGD)

fx <- runif(50,-1,-0.1)
fy <- runif(50,0,3)

sx <- runif(50, 0.1,1)
sy <- runif(50, 0,3)

fclass <- cbind.data.frame(fx, fy, 1)
sclass <- cbind.data.frame(sx, sy, -1)

tip_name <- c("one", "two", "thri")

names(fclass) <- tip_name
names(sclass) <- tip_name

for_ADALINE <- rbind.data.frame(fclass, sclass)

for_ADALINE[1,]

ggplot(for_ADALINE, aes(x = one, y = two, fill = thri)) +
  geom_point()

ADALINE_SGD <- SGD(for_ADALINE, learn_temp = 1, lyambda = 0.5, ADALINE_loss_func,
                   ADALINE_loss_func_deriv, return_all_weights =  TRUE,
                   reg_tau = 0.005, steps = 300)
ADALINE_SGD <- ADALINE_SGD[order(-ADALINE_SGD$loss), ]
ADALINE_SGD
ADALINE_draw_line(ttmp,ADALINE_SGD)
