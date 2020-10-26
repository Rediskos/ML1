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


#возвращает объект с максимальной эффективностью из xl_y
FindEtalon <- function(xl_y, xl, etalonos, lambda = 0.5) {
  #xl_y - элементы множества xl класca y  
  l <- dim(xl_y)[1]
  
  best_i <- 0
  best_eff <- -1e6
  
  #считаем обороноспособности и толерантности
  for(i in 1:l) {
    nominator <- 0
    denominator <- l - 1
    
    
    x <- xly[i, ]
    
    #оброноспособность
    for (j in 1:l) {
      
      if(j != i) {
        
        y <- xly[i, ]
        FRIS_result <- FRIS_function(y, x, NN(y, etalonos))
        
        nominator <- nominator + FRIS_result
              
      }
    }
    def_of_xi <- nominator / denominator
    
    
    #толерантность
    xl_toler <- xl[xl$Species != x$Species, ]
    nominator <- 1
    
    xl_toler_power <- dim(xl_toler)[1]
    denominator <- xl_toler_power
    
    l_toler <- xl_toler_power
    
    for(j in 1:l_toler) {
      y <- xl_toler[j, ]
      FRIS_result <- FRIS_function(y, x, NN(y, etalonos))
      
      nominator <- nominator + FRIS_result
    }
    
    toler_of_xi <- nominator / denominator
    
    xi_eff <- lambda * def_of_xi - (1 - lambda) * toler_of_xi
    
    if(xi_eff > best_eff) {
      best_eff <- xi_eff
      best_i <- i
    }
  }
  
  ans <- xl_y[best_i, ]
  
  return(ans)
}



FRiS_STOLP <- function(xl) {
  
} 