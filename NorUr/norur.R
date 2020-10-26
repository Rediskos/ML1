
#считает плотность вероятностного распределения для точки
calc_prob_rasp <- function(x, mu, E) {
  for_exp <- (t(x - mu) %*% solve(E) %*% (x - mu)) / (-2)
  nominator <- exp(for_exp)
  
  for_sqrt <- (2 * pi) ^ dim(E)[1] * det(E)
  denominator <- sqrt(for_sqrt)
  
  ans <- nominator / denominator
  return(ans)
}

#отрисовка линий уровня двумерной нормальной плотности распределения
draw_norm_lines <- function(mu, E) {
  #mu - математичское ожидание
  #E - матрица ковариации
  
  #хранит плотности распределения точек
  prob_disp <- data.frame()
  
  #границы
  l <- mu[1, 1] - 3 #левая граница
  r <- mu[1, 1] + 3 #правая граница
  t <- mu[2, 1] + 3 #верхняя граница
  b <- mu[2, 1] - 3 #нижняя граница
  
  for (i in seq(l, r, 0.1)) {
    for(j in seq(b, t, 0.1)) {
      x <- matrix(c(i, j), nrow = 2, ncol = 1)
      
      prob_rasp <- calc_prob_rasp(x, mu, E)
      
      prob_disp <- rbind(prob_disp, c(i, j, prob_rasp))
    }
  }
  
  tt <- prob_disp[order(prob_disp[,1], prob_disp[,2]),]
  
  names(tt) <- c("x", "y", "thic")
  
  contourplot(thic ~ x * y, data = tt, region = TRUE)
  
}


#n - размерность пространства
#mu - математическое ожидаение
#E - ковариационная матрица

n <- 2

# dev.new()

#признаки не коррелированы 
#1
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(0.5, 0, 0, 2), nrow = n, ncol = n)
draw_norm_lines(mu, E)

#2
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(2, 0, 0, 0.5), nrow = n, ncol = n)
draw_norm_lines(mu, E)



#признаки не коррелированы одинаковые дисперсии
#3
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(1, 0, 0, 1), nrow = n, ncol = n)
draw_norm_lines(mu, E)


#признаки корелированы
#4
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(2, 0.4, 0.4, 2), nrow = n, ncol = n)
draw_norm_lines(mu, E)

#5
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(2, -0.4, -0.4, 2), nrow = n, ncol = n)
draw_norm_lines(mu, E)

#6
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(2, 0.9, 0.9, 2), nrow = n, ncol = n)
draw_norm_lines(mu, E)

#7
mu <- matrix(c(2, 2), nrow = n, ncol = 1)
E <- matrix(c(2, -0.9, -0.9, 2), nrow = n, ncol = n)
draw_norm_lines(mu, E)
