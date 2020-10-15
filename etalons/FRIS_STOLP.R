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

#ближайший к элементу x среди множества y
NN <- function(x, y, param = NA) {
  if(is.na(param)) {
    param <- c(3:4)
  }
  
  ndist <- 100000
  nnum <- 1
  
  l <- dim(y)[1]
  
  for(i in 1:l) {
    
    tdist <- euclid(x[param, ], y[i, param])
    
    if(tdist < ndist) {
      
      ndist <- tdist
      nnum = i
    }
  }
  
  return(y[nnum, ])
}

#xly - элементы множества xl класca y
FindEtalon <- function(xly, etalonos) {
  #обороноспособность
  l <- dim(xly)[1]
  all_Dx <- data.frame()
  
  for(i in 1:l) {
    nominator <- 0
    denominator <- l - 1
    
    for (j in 1:l) {
      
      if(j != i) {
        nominator <- nominator + FRIS_function(xly[j, ], xly[i, ], )
              
      }
    }
  }
}