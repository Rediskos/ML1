
euclid <- function(x, y){
  sqrt(sum((x - y)^2))
}

distanses <- function(x, y, param = NA, metric = euclid) {
  l <- dim(y)[1]
  
  if(is.na(param)) {
    param <- c(1:4)
  }
  
  dist <- matrix(NA, l, 2)
  
  for(i in 1:l) {
    a <- c(i, metric(x[param], y[i, param]))
    dist[i, ] <- a
  }
  
  ordY <- y[order(dist[, 2]), ]
  return (ordY)
}

kwNN <- function(x, q = 0.7) {
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

kNN <- function(x, y, k = NA, q = NA, mode = simpl_kNN, rall = FALSE) {
  #возвращает класс
  
  orderY <- distanses(x, y)
  
  if(rall) {
    return(orderY)
  }
  
  res <- 0
  if(is.na(q) && !is.na(k) ) {
    
  res <- simpl_kNN(orderY[1:k, ])
  
  } else if(!is.na(q) && is.na(k)) {
    res <- kwNN(orderY, q)
  
  } else if(!is.na(q) && !is.na(k)) {
    
    res <- kwNN(orderY[1:k, ], q)
  }
  
  return(res)
}

for_plot <- function(x, d){1 - x/d}

LOO <- function(y, classifier = kNN, bounds = 1:150) {
  results <- integer(bounds[which.max(bounds)])
  l = dim(y)[1]
  for (z in 1:l) {
    d = y[z, ]
    cat(z)
    tmp <- classifier(d, iris, l, mode = simpl_kNN, rall = TRUE)
    for (i in 3:l) {
      tclass <- simpl_kNN(tmp[1:i,][-1, ])
      results[i] <- results[i] + 1 * (d$Species == tclass)
    }
  }
  print(results)
  plot(for_plot(results, 150), type = 'l', xlab = "k", ylab = "miss")
  
  return(which.max(results))
}


best_k <- LOO(iris)


LOOq <- function(y, bestk, classifier = kNN, bounds = 1:150) {
  results <- integer(100)
  l = dim(y)[1]
  
  for (z in bounds) {
    d = y[z, ]
    tmp <- classifier(d, iris, k = bestk, mode = kNN, rall = TRUE)
    
    for (i in 1:100) {
      tclass <- kwNN(tmp[1:bestk,][-1, ], i / 100)
      results[i] <- results[i] + 1 * (d$Species == tclass)
    } 
  }
  print(results)
  plot(for_plot(results, 150), type = 'l', xlab = "q", ylab = "miss")
  #print(which.max(results))
  
  
  return(which.max(results) / 100)
  #return(NA)
}

best_q <- LOOq(iris, best_k)

make_map <- function(colors, classifier = kNN, k = NA, q = NA) {
  #
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
     if(is.na(q)) {
       p$Species <- classifier(p, iris, k)
     } else {
       p$Species <- classifier(p, iris, k, q)
     }
     points(p[3], p[4], pch = 22, col = colors[p$Species], asp = 1)
     j <- j + 1/10
    }
    i <- i + 1/10
    j <- 0
  }
}

colors <- c("setosa" = "red", "versicolor" = "green3", 
            "virginica" = "blue") 
make_map(colors, k = 5)
make_map(colors, k = best_k)
make_map(colors, q = best_q)
make_map(colors, k = 5, q = 0.7)
make_map(colors, k = 7, q = best_q)



x <- iris[sample(1:150, 10), ]
x$Species <- NA


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


make_map(colors, k = best_k)

test <- iris[c(1:7),]
test
test[c(5:7), 5] = "virginica" 

test[c(1:4), 3] = 1
test
for(i in 1:4) {
  test[i, 4] = i
}
test

test[c(5:7), 3] = 6
test
for(i in 2:4) {
  test[3 + i, 4] = i
}
test
x <- test
plot(test[,3], test[,4], pch = 22, bg = colors[test$Species], xlim=c(0, 6), ylim=c(0, 5),
       col = colors[test$Species], asp = 3, cex = 5, xlab = "x", ylab = "y")

test
ans <- vector()
for(i in 1:dim(test)[1]) {
  # ans <- c(ans, kNN(test[i,], test, 7))
  ans <- c(ans, kNN(test[i,], test, 7, q = 0.5))
}

plot(test[,3], test[,4], pch = 22, bg = colors[test$Species], xlim=c(0, 6), ylim=c(0, 5),
     col = colors[test$Species], asp = 3, cex = 5, xlab = "x", ylab = "y")
ans

x$Species <- ans
x
points(x[,3], x[,4], pch = 23,bg = colors[x$Species],
       col = colors[x$Species], cex = 5)
