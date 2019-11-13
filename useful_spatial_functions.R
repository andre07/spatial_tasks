
mtx_power <- function (X, n) {
  require(Matrix)
  if (n != round(n)) {
    n <- round(n)
    warning("rounding exponent `n' to", n)
  }
  
  M <- Matrix(X, sparse = TRUE)
  
  M_tmp <- M
  
  if (n >= 2) {
    for (j in 1:(n-1)) {
      M_tmp <- M_tmp %*% M
    }  
  }
  return(M_tmp)
}

spatial_k_level_neighbour <- function(id, m, k) {
  #id - for which municipality we would like to have neighbours
  #m - neighbourhood matrix
  #k - level of neighbourhood
  #function returns the list of ids for neighbours of levels from 1 to k

  mat_k <- mtx_power(m, k) %>% as.matrix()  
  
  
  out <- vector(mode = "list", length = length(id))
  names(out) <- paste0("id_", id)
  for (j in 1:length(id)) {
    v <- rep(0, nrow(m))
    v[id[j]] <- 1
    
    temp <- v %*% mat_k
    
    out[[j]] <- which(temp > 0) 
  }
  
  return(out)
}

spatial_k_level_neighbour(id = 1, m, 1)
