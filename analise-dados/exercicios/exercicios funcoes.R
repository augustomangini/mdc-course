mymin <- function(...) {
  min <- Inf
  for (i in c(...)) {
    if (i < min) {
      min <- i
    }
  }
  return(min)
}

subset <- function(v1, v2) {
  result <- vector()
  for (i in v1) {
    found <- FALSE
    for (x in v2) {
      if (x == i) {
        found <- TRUE
      }
    }
    result <- c(result, found)
  }
  return(all(result))
}


myprod <- function(...) {
  v <- c(...)
  prod <- 1
  for (i in v) {
    prod <- prod * i
  }
  return(prod)
}



mymedian <- function(...) {
  v <- c(...)
  soma <- 0
  for (i in v) {
    soma <- soma + i
  }
  return(soma/length(v))
}


# funcao index(),
index <- function(vector, element) {
  result <- NULL
  for (i in seq_along(vector)) {
    if (vector[i] == element) {
      result <- c(result, i)
    }
  }
  return(result)
}


mycount <- function(vector, element) {
  result <- 0
  for (i in seq_along(vector)) {
    if (vector[i] == element) {
      result <- result + 1
    }
  }
  return(result)
}

myunique <- function(...) {
  result <- NULL
  for (i in c(...)) {
    if (any(result == i) == FALSE) {
      result <- c(result, i)
    }
  }
  return(result)
}

mymode <- function(...) {
  v <- c(...)
  
  # Obter elementos únicos
  unique_elements <- myunique(v)
  
  # Encontrar a frequência máxima
  max_freq <- 0
  for (i in unique_elements) {
    freq <- mycount(v, i)
    if (freq > max_freq) {
      max_freq <- freq
    }
  }
  
  # Retornar elemento(s) com frequência máxima
  result <- NULL
  for (i in unique_elements) {
    if (mycount(v, i) == max_freq) {
      result <- c(result, i)
    }
  }
  
  return(result)
} 

# ......

gcd2 <- function(x, y) {
  if (y == 0) {
    return(x)
  } else {
    return(gcd2(y, x %% y))
  }
}


mysort <- function(...) {
  v <- c(...)
  result <- NULL
  for (i in 1:(length(v) - 1)) {
    if (v[i] > v[i + 1]) {
      result <- c(result, v[i+1], v[i])
    } else {
      result <- c(result, v[i], v[i+1])
    }
  }
  return(result)
}
