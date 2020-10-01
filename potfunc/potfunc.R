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

check_window <- function(x, y, h, param = NA) {
  cnt <- integer(3)
  names(cnt) <- names(table(y$Species))
  
  i = 1;
  l = dim(y)[1]
  
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  
  while (euclid(x[param], y[i, param]) < h && i <= l) {
    cnt[y$Species[i]] <- cnt[y$Species[i]] + y$potenc[i]
    i <- i + 1
  }
  
  if(sum(cnt) == 0) {
    return("na")
  }
  
  return(names(which.max(cnt)))
}

parsewindow <- function(x, y, h, metrica = distanses, param = NA, rall = FALSE) {
  ordY <- distanses(x, y)
  
  if(rall) {
    return(ordY)
  }
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  
  return(check_window(x, ordY, h, param = param))
}


potfuc <- function(y, missrate = 0.05, h = best_h) {
  
  
  
  l = dim(y)[1]
  results <- 1
  miss = 1
  while(miss > missrate) {
    cat("\r",fill = TRUE)
    cat("\r")
    cat(miss)
    miss = 1
    for (i in 1:l) {
      d <- y[i, ]
      tclass <- parsewindow(d, y, best_h)
      
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
tmp <- potfuc(pfield)

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

make_map <- function(colors = NA, classifier = parsewindow, h = 2) {
  #
  
  if(is.na(colors)) {
    colors <- c("setosa" = "red", "versicolor" = "green3", 
                "virginica" = "blue", "na" = "yellow") 
  }
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
  points(iris[, 3:4], pch = 22, bg = colors[iris$Species], col 
         = colors[iris$Species], asp = 1)
  
  i = 0
  j = 0
  while (i <= 7) {
    while(j < 2.6) {
      p <- iris[1, ]
      p[3] = i
      p[4] = j
      p$Species <- classifier(p, iris, h)
      points(p[3], p[4], pch = 22, col = colors[p$Species], asp = 1)
      j <- j + 1/10
    }
    i <- i + 1/10
    j <- 0
  }
}



make_map()
make_map(h = best_h)

pfield <- data.frame(cbind(integer(150)))
pfield
names(pfield) <- "potenc"

pfield <- cbind(iris, pfield)


pfield

