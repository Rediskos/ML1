
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

kwNN <- function(x) {
  q = 0.7;
  cnt <- c(0, 0, 0)
  names(cnt) <- names(table(x$Species))
  l <- dim(x)[1]
  
  for (i in 1:l) {
    cnt[x$Species[i]] <- cnt[x$Species[i]] + 1 * (q ^ (i - 1))
  }
  
  return(names(which.max(cnt)))
}

simpl_kNN <- function(x) {
  tmp <- table(x$Species)
  return(names(which.max(tmp)))
}

kNN <- function(x, y, k, mode = simpl_kNN, rall = FALSE) {
  #возвращает класс
  
  orderY <- distanses(x, y)
  
  if(rall) {
    return(orderY)
  }
  
  n <- dim(orderY)[2]
  return(mode(orderY[1:k, ]))
}

for_plot <- function(x, d){x/d}

LOO <- function(y, classifier = kNN, bounds = 1:150) {
  results <- integer(bounds[which.max(bounds)])
  l = dim(y)[1]
  for (z in 1:l) {
    d = y[z, ]
    cat(z)
    tmp <- classifier(d, iris, l, mode = kwNN, rall = TRUE)
    for (i in 1:l) {
      tclass <- kwNN(tmp[1:i,][-1, ])
      results[i] <- results[i] + 1 * (d$Species == tclass)
    }
  }
  print(results)
  plot(for_plot(results, 150), type = 'l')
  
  return(which.max(results))
}


best_k <- LOO(iris)

make_map <- function(colors, classifier = kNN, k = 5) {
  #
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
  i = 0
  j = 0
  while (i < 10) {
    while(j < 2.5) {
     p <- iris[1, ]
     p[3] = i
     p[4] = j
     p$Species <- classifier(p, iris, k)
     points(p[3], p[4], pch = 22, bg = colors[p$Species], col = colors[p$Species], asp = 1)
     j <- j + 2/10
    }
    i <- i + 2/10
    j <- 0
  }
}



x <- iris[sample(1:150, 10), ]
x$Species <- NA

colors <- c("setosa" = "red", "versicolor" = "green3", 
            "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col 
     = colors[iris$Species], asp = 1)

ans <- vector()
for(i in 1:dim(x)[1]) {
  ans <- c(ans, kNN(x[i,], iris, 5))
}

str(ans)

x$Species <- ans
x
points(x[,3], x[,4], pch = 22, bg = colors[x$Species],
       col = colors[x$Species])


make_map(colors(k = best_k))
