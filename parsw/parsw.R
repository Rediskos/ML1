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
    
    cnt[y$Species[i]] <- cnt[y$Species[i]] + kernel(tmp, h)
    
    i <- i + 1
  }
  
  if(sum(cnt) == 0) {
    return("na")
  }
  
  return(names(which.max(cnt)))
}

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

for_plot <- function(x, d){1 - x/d}

LOO <- function(y, classifier = parsewindow, bounds = 1:150, param = NA, kernel = kernel_square) {
  
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  results <- integer(300)
  l = dim(y)[1]
  
  for (z in 1:l) {
    d = y[z, ]
    cat(z)
    tmp <- classifier(d, iris, l, rall = TRUE, kernel = kernel)
    
    cnt <- integer(3)
    names(cnt) <- names(table(y$Species))
    h = 30
    ptr = 2
    
    while(h <= 300 && ptr <= l) {

      if(euclid(d[3:4], tmp[ptr, 3:4]) > h / 100) {
        tmp_class <- names(which.max(cnt))
        results[h] <- results[h] + 1 * (tmp_class == d$Species)
        h <- h + 1
      }

      cnt[tmp$Species[ptr]] <- cnt[tmp$Species[ptr]] + 1
      ptr <- ptr + 1
    }
    
  }
  
  print(results)
  i <- seq(from = 0.3, to = 3, by = 0.01)
  plot(i, for_plot(results[i * 100] , 150), type = 'l', xlab = "h", ylab = "miss")
  
  return(which.max(results) / 100)
}


best_h <- LOO(iris)

make_map <- function(colors = NA, classifier = parsewindow, h = 2, kernel = kernel_square) {
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
      p$Species <- classifier(p, iris, h, kernel = kernel)
      points(p[3], p[4], pch = 22, col = colors[p$Species], asp = 1)
      j <- j + 1/10
    }
    i <- i + 1/10
    j <- 0
  }
}




make_map(h = 0.1, kernel = kernel_gaus)
make_map(h = 0.3,  kernel = kernel_gaus)
make_map(h = 2, kernel = kernel_gaus)

x <- iris[sample(1:150, 10), ]
x
x$Species <- NA

l = 10

ans <- vector()
for(i in 1:dim(x)[1]) {
  ans <- c(ans, parsewindow(x[i,], iris, 1))
}
ans
str(ans)

x$Species <- ans
x
