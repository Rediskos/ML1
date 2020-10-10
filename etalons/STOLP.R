
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

simpl_marg_knn <- function(x, t) {
  tmp <- table(x$Species)
  z = tmp[names(tmp) != t]
  return( tmp[t] - z[which.max(z)] )
}

marg_knn <- function(x, y, k, chk_fst = FALSE) {
  ordY <- distanses(x, y)
  
  # print(x)
  # print(ordY[1,])
  if(chk_fst && prod(x[1, ] == ordY[1, ])) {
    return("kek")
  }
  
  return(simpl_marg_knn(ordY[1:k, ], x$Species))
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

LOO <- function(x, y, classifier = kNN, bounds = 1:150) {
  results <- integer(bounds[which.max(bounds)])
  l = dim(y)[1]
  lx = dim(x)[1]
  for (z in 1:lx) {
    d = x[z, ]
    cat(z)
    tmp <- classifier(d, y, l, mode = simpl_kNN, rall = TRUE)
    for (i in 1:l) {
      tclass <- simpl_kNN(tmp[1:i,][-1, ])
      results[i] <- results[i] + 1 * (d$Species == tclass)
    }
  }
  print(results)
  plot(for_plot(results, 150), type = 'l', xlab = "k", ylab = "miss")
  
  return(which.max(results))
}


best_k <- LOO(iris, s)

make_map <- function(st, colors = NA, classifier = kNN, h = 2) {
  #
  print(st)
  
  if(is.na(colors)) {
    colors <- c("setosa" = "red", "versicolor" = "green3", 
                "virginica" = "blue", "na" = "yellow") 
  }
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
  points(iris[, 3:4], pch = 21, bg = colors[iris$Species],
         col = colors[iris$Species], asp = 1, cex = 0.5)
  
  points(st[, 3:4], pch = 22, 
         bg = colors[st$Species], col 
         = colors[st$Species], asp = 1)
  i = 0
  j = 0
  while (i <= 7) {
    while(j < 2.6) {
      p <- st[1, ]
      p[3] = i
      p[4] = j
      p$Species <- classifier(p, st, k = min(best_k, 7))
      points(p[3], p[4], pch = 21, col = colors[p$Species], asp = 1)
      j <- j + 1/10
    }
    i <- i + 1/10
    j <- 0
  }
}
make_map(s)


make_STOLP_map <- function(st, y = iris, colors = NA, classifier = parsewindow, h = 2) {
  #
  
  if(is.na(colors)) {
    colors <- c("setosa" = "red", "versicolor" = "green3", 
                "virginica" = "blue", "na" = "yellow") 
  }
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 7), ylim=c(0, 2.5))
  points(y[, 3:4], pch = 22, col 
         = colors[y$Species], asp = 1)
  
  for(i in 1:dim(st)[1]) {
    p <- st[i, ]
    points(p[3], p[4], pch = 22,  bg = colors[p$Species], col = colors[p$Species], asp = 1)
  }
  
}

after_STOLP <- data.frame()


for(i in 1:dim(iris)[1]) {
  
  t <- iris[i, ]
  
  mt <- marg_knn(t, iris, 7)
  mt <- data.frame(mt)
  
  names(mt) <- "margin"
  kkk = rbind(after_STOLP, cbind(t, mt))
  if(mt >= 0) {
    
    
    after_STOLP <- rbind(after_STOLP, cbind(t, mt))

  }
}

dim(after_STOLP)[1]

after_STOLP


make_STOLP_map(after_STOLP)

fmm <- function(x) {return(which(iris$Species == x))}

nm <- levels(iris$Species)

s <- data.frame()

for(i in nm) {
  print("")
  print(i)
  tmp <- after_STOLP[after_STOLP$Species == i, ]
  print(tmp)
  ptr <- which.max(tmp$margin)
  s <- rbind(s, tmp[ptr,])
}
s


make_STOLP_map(s, after_STOLP)

while(length(s[,1]) < length(after_STOLP[,1])) {
  cnt <- 0
  l = dim(after_STOLP)[1]
  minm <- 10000
  xi = 1
  
  for(i in 1:l) {
    p <- after_STOLP[i, ]
    
    pmarg <- marg_knn(p, s, min(length(s), 7), chk_fst = TRUE)
    print(pmarg)
    if(pmarg != "kek" && pmarg < 1) {
      cnt <- cnt + 1
    } 
    
    if(pmarg != "kek" && pmarg < minm) {
      minm <- pmarg
      xi = i
    }
  }
  
  if(cnt < 1 && length(s[,1]) > 7) {
   break 
  }
  
  s <- rbind(s, after_STOLP[xi, ])
}

s


make_STOLP_map(s)
