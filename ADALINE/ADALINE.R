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
  
}


fx <- runif(50,-2,-1)
fy <- runif(50,1,2)

sx <- runif(50, 1,2)
sy <- runif(50, 3,4)

fclass <- cbind.data.frame(fx, fy, 1)
sclass <- cbind.data.frame(sx, sy, -1)

tip_name <- c("one", "two", "thri")

names(fclass) <- tip_name
names(sclass) <- tip_name

for_ADALINE <- rbind.data.frame(fclass, sclass)

for_ADALINE[1,]

ggplot(for_ADALINE, aes(x = one, y = two, fill = thri)) +
  geom_point()

ADALINE_SGD <- SGD(for_ADALINE, 0.1, 0.8, ADALINE_loss_func,
                   ADALINE_loss_func_deriv, return_all_weights =  TRUE,
                   reg_tau = 10)
