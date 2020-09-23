oneNN <- function(data) {
  answers <- vector()
  print(answers)
  for (r in 1:dim(data)[1]) {
    d <- data[r,]
    clen <- 1e10
    closest <- iris[1,]
    
    for (z in 1:150) {
      i <- iris[z,]
      tmp <- sqrt((d[1,3] - i[1,3])^2 + (d[1,4] - i[1,4])^2)
      # print(tmp)
      # print(clen)
      if (clen > tmp) {
        clen <- tmp
        closest <- iris[z,]
      }
    }
    answers <- c(answers, levels(closest$Species)[as.numeric(closest$Species)])
  }
  
  print(answers)
  
  data$Species <- factor(answers, levels = levels(iris$Species)) 
  return(data)
}

x <- iris[sample(1:150, 5),]
x[3:4] <- x[3:4] + 1
x["Species"] <- "None"
x
x <- oneNN(x)
x

colors <- c("setosa" = "red", "versicolor" = "green", "verginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col 
     = colors[iris$Species]) 
points(x[, 3], x[, 4], pch = 22, bg = colors[x$Species], col 
       = colors[x$Species])
