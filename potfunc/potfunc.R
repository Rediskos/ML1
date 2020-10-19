euclid <- function(x, y){
  sqrt(sum((x - y)^2))
}

distanses <- function(x, y, param = NA, metric = euclid) {
  l <- dim(y)[1]
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  dist <- matrix(NA, l, 2)
  
  for(i in 1:l) {
    a <- c(i, metric(x[param], y[i, param]))
    dist[i, ] <- a
  }
  
  ordY <- y[order(dist[, 2]), ]
  return (ordY)
}

kernel_square <- function(p, h) {
  ans <- 0
  
  if(p/h <= 1) {
    ans <- 0.5
  }
  
  return(ans)
}

kernel_triangle  <- function(p, h) {
  ans <- max(0, 1 - p/h)
  
  
  return(ans)
}

kernel_gaus <- function(p, h) {
  ans <- (1 / (sqrt(2 * pi))) * (exp(-((p/h) * (p/h)) / 2)) 
  
  
  return(ans)
}

check_window <- function(x, y, h, param = NA, kernel = kernel_square) {
  cnt <- integer(3)
  names(cnt) <- names(table(y$Species))
  
  i = 1;
  l = dim(y)[1]
  
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  while (i <= l) {
    tmp <- euclid(x[param], y[i, param])
    
    cnt[y$Species[i]] <- cnt[y$Species[i]] + y$potenc[i] * kernel(tmp, h)
    
    i <- i + 1
  }
  
  if(sum(cnt) == 0) {
    return("na")
  }
  
  return(names(which.max(cnt)))
}


make_map(tmp, h = best_h)


parsewindow <- function(x, y, h, metrica = distanses,
                        param = NA, rall = FALSE, kernel = kernel_square) {
  ordY <- distanses(x, y)
  
  if(rall) {
    return(ordY)
  }
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  return(check_window(x, ordY, h, param = param, kernel = kernel))
}


potfuc <- function(y, h = best_h, kernel = kernel_square) {
  
  
  
  l = dim(y)[1]
  results <- 1
  miss = 1
  pmiss = 1
  cnt = 0
  
  while(cnt < 3) {
    cat(miss, fill = TRUE)
    
    if(miss - pmiss < 1e-6) {
      cnt <- cnt + 1
    } else {
      cnt = 0
    }
    pmiss = miss
    
    miss = 1
    for (i in 1:l) {
      d <- y[i, ]
      tclass <- parsewindow(d, y, best_h, kernel = kernel)
      
      if(tclass != d$Species) {
        y$potenc[i] <- y$potenc[i] + 1
      } else {
        miss <- miss - 1/150
      }
    }
    results <- c(results, miss)
  }
  
  plot(results, type = 'l', xlab = "step", ylab = "miss")
  return(y)
}

pfield
tmp <- potfuc(pfield, kernel = kernel_gaus)
colors <- c("setosa" = "red", "versicolor" = "green3", 
            "virginica" = "blue", "na" = "yellow") 
tmp




for_plot <- function(x, d){1 - x/d}

LOO <- function(y, classifier = parsewindow, bounds = 1:150, param = NA, potenc) {
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  results <- integer(300)
  l = dim(y)[1]
  
  for (z in 1:l) {
    d = y[z, ]
    cat(z)
    tmp <- classifier(d, iris, l, rall = TRUE)
    
    cnt <- integer(3)
    names(cnt) <- names(table(y$Species))
    h = 10
    ptr = 2
    
    while(h <= 300 && ptr <= l) {
      
      if(euclid(d[3:4], tmp[ptr, 3:4]) > h / 100) {
        tmp_class <- names(which.max(cnt))
        results[h] <- results[h] + 1 * (tmp_class == d$Species)
        h <- h + 1
      }
      
      cnt[tmp$Species[ptr]] <- cnt[tmp$Species[ptr]] + potenc[i]
      ptr <- ptr + 1
    }
    
  }
  
  i <- seq(from = 0.01, to = 3, by = 0.01)
  plot(i, for_plot(results[i * 100] , 150), type = 'l', xlab = "h", ylab = "miss")
  
  return(which.max(results) / 100)
}


best_h <- LOO(iris)

plot(tmp$Petal.Length, tmp$Petal.Width, pch = 21,
     bg = colors[tmp$Species], col 
     = colors[tmp$Species], xlab = "Petal Length", ylab = "Petal Width")
points(tmp$Petal.Length, tmp$Petal.Width, pch = 21,
       bg = colors[tmp$Species], col 
       = "black" ,  cex = tmp$potenc / 3 + 1)


make_map <- function(y, colors = NA, classifier = parsewindow, h = 2, kernel = kernel_square) {
  #
  
  if(is.na(colors)) {
    colors <- c("setosa" = "red", "versicolor" = "green3", 
                "virginica" = "blue", "na" = "yellow") 
  }
  plot(1, type="n", xlab = "Petal Length", ylab = "Petal Width", xlim=c(0, 7), ylim=c(0, 2.5))
  
  points(tmp$Petal.Length, tmp$Petal.Width, pch = 21,
       bg = colors[tmp$Species], col 
       = colors[tmp$Species], xlab = "Petal Length", ylab = "Petal Width")
  points(tmp$Petal.Length, tmp$Petal.Width, pch = 21,
         bg = colors[tmp$Species], col 
         = "black" ,  cex = tmp$potenc / 3 + 1)
  
  i = 0
  j = 0
  while (i <= 7) {
    while(j < 2.6) {
      p <- y[1, ]
      p[3] = i
      p[4] = j
      p$potenc = 0
      p$Species <- classifier(p, y, h, kernel = kernel)
      points(p[3], p[4], pch = 21, col = colors[p$Species], asp = 1)
      j <- j + 1/10
    }
    i <- i + 1/10
    j <- 0
  }
}



make_map(tmp, h = best_h, kernel = kernel_gaus)

pfield <- data.frame(cbind(integer(150)))
pfield
names(pfield) <- "potenc"

pfield <- cbind(iris, pfield)


pfield

