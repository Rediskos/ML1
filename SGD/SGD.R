library(ggplot2)
#функция нормализации
normilize_X <- function(x) {
  m <- dim(x)[2]
  
  if(is.null(m)) {
    m <- dim(x)[1]
    minv <- min(x)
    maxv <- max(x)
    
    if(maxv - minv < 1e-8) {
      x <- rep(0, m)
    } else {
      
      x <- (x - minv) / (maxv - minv)
    }
    
  } else {
    for(i in 1:m) {
      minv <- min(x[,i])
      maxv <- max(x[,i])
      if(maxv - minv < 1e-8) {
        x[,i] <- 0
      } else {
        
        x[,i] <- (x[,i] - minv) / (maxv - minv) 
      }
    }
  }
  
  # x[,1] <- x[,1] - 0.5
  
  return(x)
}

norm_vec <- function(x) sqrt(sum(x^2))

def_leanr_rate_calc <- function(x, t) {
  response <- 1/t
  return(response)
}

SGD_finde_worst <- function(w, x, y, not_this_class, loss_func) {
  l <- dim(x)[1]
  
  max_loss <- 0
  max_loss_idx <- 1
  
  for(i in 1:l) {
    xi <- x[i,]
    yi <- y[i,]
    
    loss <- loss_func(w, xi, yi)
    
    if(loss > max_loss && yi != not_this_class) {
      max_loss <- loss
      max_loss_idx <- i
    }
    
  }
  
  return(max_loss_idx)
}

SGD_draw_weights <- function(xl, w1, w2, step, xi) {
  
  tx <- data.frame(
    one = xi[2],
    two = xi[3]
  )
  
  
  xl_names <- names(xl)
  a1 = -w1[1]/w1[3]
  b1 = -w1[2]/w1[3]
  
  aa <- data.frame(
    xx = a1,
    yy = b1
  )
  
  
  a2 = -w2[1]/w2[3]
  b2 = -w2[2]/w2[3]
  
  
  bb <- data.frame(
    xx = a2,
    yy = b2
  )
  
  my.scales <- list(
    scale_x_continuous(name="x"),
    scale_y_continuous(name="y")
    # xlim(-3, 3),
    # ylim(-3, 3)
  )
  
 p <- ggplot()
 p <- p + geom_point(xl, mapping = aes(x = one, y = two, fill = thri), size = 2,
               stroke = 1, shape = 21)
 p <- p + scale_color_manual(labels = c("после SGD шага", "до SGD шага"),
                       values =  c("past" = "black",
                                  "now" = "green"), name = "Разделяющая прямая")
 p <- p + geom_point(data = tx, aes(x = one, y = two, fill = "Проверяемая точка"),
               shape = 21, size = 5)
 p <- p + scale_fill_manual("Класс",
                       values = c("yellow", "red","blue"))
 p <- p + geom_abline(aa, mapping = aes(slope = yy, intercept = xx, color = "past"),
                size = 2) 
 p <- p + geom_abline(bb, mapping = aes(slope = yy, intercept = xx, color = "now"),
                size = 2)
 p <- p + my.scales
 
 file_path = "E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/LogReg/aaa"
 file_name <- paste(step, "png", sep = ".")
 # print(p)
 ggsave(filename = file_name, path = file_path, plot = p)
}


#функция стохастического градиентного спуска
SGD <- function(xl, learn_temp_func = def_leanr_rate_calc,
                lyambda, los_func, los_func_deriv,
                return_all_weights = FALSE, reg_tau = 0.5, steps = 200,
                normIt = TRUE) {
  #xl - датафрейм векторов выборки
  #learn_temp - темп обучения
  #lyambda - параметр сглаживания
  #los_func - функция потерь
  #los_func_deriv - производная функции потерь
  #return_all_weights - TRUE, если требуется вернуть не просто веса, а историю изменения
  #reg_tau - параметри регуляризации
  #steps - максимальное колличество шагов алгоритма
  #normIt - TRUE, если требуется нормализовать признаки
  
  l <- dim(xl)[1] #размер выборки
  
  
  
  ml <- dim(xl)[2] #сколько типов признаков
  nms <- c("shift", names( xl[, 1:ml-1]))
  # nms <- names( xl[, 1:ml-1])
  
  x <- as.matrix(xl[, 1:ml-1], ncol = ml-1) #матрица параметров
  y <- xl[, ml] #матрица ответов
  
  ty <- levels(y)[y]
  ty <- as.numeric(ty)
  
  y <- matrix(ty, ncol = 1)
  
  
  if(normIt == TRUE) {
    x <- normilize_X(x)
  }
  
  global_x <- x
  
  add_x <- matrix(rep(1, l), ncol = 1)#столбец для смещений
  
  
  xplot <- cbind.data.frame(x, as.factor(y))
  xplot <- as.data.frame(xplot)
  x <- cbind(add_x, x)
  
  
  xz <- cbind.data.frame(x,y)
  m <- dim(x)[2]
  w <- runif(m , -1/(2*l), 1/(2*l)) #начальные веса
  names(w) <- nms
  names(xz) <- c("shift", names(xl))
  xz$thri <- xl$thri
  
  Q <- 0 #функционал ошибки
  
  #начальная ошибка
  for(i in 1:l) {
    xi <- x[i,]
    yi <- y[i,]
    
    los_func_res <- los_func(w, xi, yi)
    
    Q <- Q + los_func_res
  }
  
  Q_past <- Q + 10000#значение ошибки с предыдущего шага.
  
  wt <- c(w, Q)#веса вместе с ошибкой на них
  
  response <- wt #возвращаемый из функции ответ
  tabs <- 1
  
  
  
  
  step = 1
  # pathh = "E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/Hebb/aaa"
  # pathh <- paste(pathh, step, sep = "/")
  # pathh <- paste(pathh, "pdf", sep = ".")
  # 
  # pdf(file = pathh)
  for_learn_temp = 1
  
  yi = 0
  
  colorr <- c( "1" = "red", "-1" = "yellow")
  
  while ((Q > 0.1 || tabs > 0.1) && step <= steps) {
    # if(step > 5) {
    #   print("kek")
    # }
    max_loss_idx <- SGD_finde_worst(w, x, y, yi, loss_func = los_func)
    
    xi <- x[max_loss_idx, ]
    yi <- y[max_loss_idx, ]
    
    
    
    
    learn_temp <- learn_temp_func(xi, for_learn_temp)#высчитать темп обучения
    
    res_from_los_deriv <- los_func_deriv(w, xi, yi)
    
    for_weight_step <- res_from_los_deriv
    
    
    
    res_from_los <- los_func(w, xi, yi) + reg_tau / 2 * (norm_vec(w) ^ 2)
    
    Q_past <- Q
    Q <- (1 - lyambda) * Q + lyambda * res_from_los
    
    w1 <- w
    
    wt <- c(w, Q)
    
    # ggplot()
    # 
    # pathh = "E:/.Учёба/СЕЙЧАС/..Системы и методы принятия решений/pr/ML1/Hebb/aaa"
    # pathh <- paste(pathh, step, sep = "/")
    # pathh <- paste(pathh, "pdf", sep = ".")
    # 
    # cairo_pdf(file = pathh)
    # par(xpd=FALSE)
    # plot(xplot[, 1:2], pch = 21, xlim = c(0, 1), ylim = c(0,1),
    #      bg = colorr[as.numeric(xplot$`as.factor(y)`)])
    # points(xi[2], xi[3], pch = 22, bg = "blue", cex = 3)
    # abline(a = -w[1]/w[3], b = -w[2]/w[3])
    
    
    yit <- t(w) %*% xi
    
    if(return_all_weights == TRUE) {
      response <- rbind.data.frame(response, wt)
    } else {
      response <- wt
    }
    
    
    tabs <- abs(Q_past - Q)
    
    if(res_from_los - reg_tau / 2 * (norm_vec(w) ^ 2) > 1 && for_learn_temp > 1) {
      w <- w*(1 - learn_temp*reg_tau) - learn_temp * for_weight_step
    }
    
    for_learn_temp <- for_learn_temp + 1
    
    # abline(a = -w[1]/w[3], b = -w[2]/w[3], col = "green")
    # 
    # legend(0, 0, pch = c(21, 21, NA, 22, NA),
    #        pt.bg = c("yellow", "red", NA,  "blue", NA, "black", "green"),
    #        legend = c("-1", "1", NA, "проверяемый елемент", NA,
    #                   "прошлая разд. прямая",
    #                   "текущая разд. прямая"),
    #        lty = c(NA, NA, NA, NA, NA, 1, 1), 
    #        pt.cex = 2,
    #        cex = 1.1,
    #        text.font = 20,
    #        bg = "white",
    #        text.width = 0.4,
    #        y.intersp = 0.8
    # )
    # 
    # dev.off()
    
    # dev.new("windows")
    # plot(xplot[, 1:2], pch = 21, xlim = c(0, 1), ylim = c(0,1),
    #      bg = colorr[as.numeric(xplot$`as.factor(y)`)])
    # points(xi[2], xi[3], pch = 22, bg = "blue", cex = 3)
    # abline(a = -w[1]/w[3], b = -w[2]/w[3])
    # legend("topright", pch = c(21, 21, NA, 22, NA),
    #        pt.bg = c("yellow", "red", NA,  "blue", NA, "black", "green"),
    #        legend = c("класс 1", "класс 2", NA, "проверяемый елемент", NA,
    #                   "прошлая разд. прямая",
    #                   "текущая разд. прямая"),
    #        lty = c(NA, NA, NA, NA, NA, 1, 1),
    #        pt.cex = 2,
    #        cex = 1.2,
    #        text.width = 0.2,
    #        y.intersp = 0.7
    #        )
    # dev.off()
    
    w2 <- w
    
    SGD_draw_weights(xz, w1, w2, step, xi)
    step <- step + 1
  }
  
  names(response) <- c(nms, "loss")
  return(response)
}

