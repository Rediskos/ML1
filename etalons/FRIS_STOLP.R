euclid <- function(x, y){
  sqrt(sum((x - y)^2))
}

FRIS_function <- function(u, x, d, param = NA, metric = euclid) {
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  nominator <- metric(u[param, ], d[param,]) - metric(u[param, ], x[param, ])
  denominator <- metric(u[param, ], d[param,]) + metric(u[param, ], x[param, ])
  
  return(nominator / denominator)
}

#xly - элементы множества xl класa y
FindEtalon <- function(xly, etalonos) {
  
}