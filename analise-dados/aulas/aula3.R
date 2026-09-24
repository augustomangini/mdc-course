# Definindo Funções

mysum <- function(x, y) {
  x + y
}

class(mysum)

is.function(mysum)

mysum

mysum(2, 5)

mysum(1:3, 8:10)

makePower <- function(n) {
  function(x) {
    x^n
  }
}

square <- makePower(2)
cube <- makePower(3)

formals(matrix)

subvector <- function(vector, begin = 1, end = length(vector)) {
  return(vector[begin:end])
}

subvector(1:10, begin = 5)
subvector(vec = 1:10, end = 5)
subvector(begin = 3, end = 6, 1:10)

mydist <- function(p1 = c(0, 0), p2 = c(0, 0)) {
  sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
}

mydist(p1 = c(4,3))
mydist(p2 = c(24,7))
mydist(p2 = c(2,3), p1 = c(7,15))

truncd <- function(n, digits = 0) {
  trunc(n * 10^digits) / 10^digits
}

truncd(123.456)
truncd(123.456, digits = 1)
truncd(123.456, d = 2)

mylength <- function(...) {
  length(c(...)) 
}

mylength(3, 2, 4)
mylength(3, 2, 4, 1:5)

mylength <- function(...) {
  length(list(...)) 
}

mylength(3, 2, 4)
mylength(3, 2, 4, 1:5)

# Escopo

search()

length <- rev

length(c(4:9))

base::length(c(4:9))

rm(length)

length(c(4:9))

length(ls(name = "package:base"))

length(ls(name = "package:stats"))

# Comandos Condicionais

odd <- function(x) {
  if (x %% 2 == 1) return(TRUE)
  if (x %% 2 == 0) return(FALSE)
}

odd <- function(x) {
  if (x %% 2 == 1) {
    TRUE
  } else {
    FALSE
  }
}

odd <- function(x) {
  ifelse(x %% 2 == 1, TRUE, FALSE)
}

odd <- function(x) {
  x %% 2 == 1
}

odd <- function(x) {
  return(x %% 2 == 1)
}

mymin <- function(a, b) {
  if (a < b) return(a)
  else return(b)
}

mymin <- function(a, b) {
  return(ifelse(a < b, a, b))
}

myabs <- function(a) {
  if (a < 0) {
    return(-a)
  } else {
    return(a)
  }
}

myabs <- function(a) {
  if (a < 0) {
    -a
  } else {
    a
  }
}

myabs <- function(a) {
  return(ifelse(a < 0, -a, a))
}

bhaskara <- function(a = 0, b = 0, c = 0) {
  if (a != 0) {
    delta <- as.complex(b^2 - 4*a*c)
    if (delta != 0) {
      return(c((-b + sqrt(delta)) / (2 * a), 
               (-b - sqrt(delta)) / (2 * a)))
    } else {
      return(-b / (2 * a))
    }
  } else {
    return(-c / b)
  }
}

myround <- function(n, digits = 0, signif) {
  if (missing(signif)) {
    round(n, digits)
  } else {
    n <- trunc(n * 10^signif)
    n <- round(n, -(signif - digits))
    n / 10^signif
  }
}

x <- (20.5:30)/10; x
print(x, 20)
round(x, 1)
myround(x, 1, 2)

# Comandos de Repeticao

printVector <- function(vector) {
  i <- 1
  while (i <= length(vector)) {
    print(vector[i])
    i <- i + 1
  }
}

printVector <- function(vector) {
  for (i in vector) {
    print(i)
  }
}

mysum <-function(...) {
  k <- 0
  for (i in c(...)) {
    k <- k + i
  }
  return(k)
}

mylength <- function(vector) {
  k <- 0
  for (i in vector) {
    k <- k + 1
  }
  return(k)
}

mylength <- function(...) {
  k <- 0
  for (i in c(...)) {
    k <- k + 1
  }
  return(k)
}

multlength <- function(...) {
  result <- NULL
  for (i in list(...)) {
    result <- c(result, length(i))
  }
  return(result)
}

multlength <- function(...) {
  result <- NULL
  k <- 0
  for (i in list(...)) {
    k <- k + 1
    result[k] <- length(i)
  }
  return(result)
}

multlength(25:30, matrix(1:12, 3, 4), 
           rnorm(5), sample(10))

mymin <- function(...) {
  min <- Inf
  for (i in c(...)) {
    if (i < min) {
      min <- i
    }
  }
  return(min)
}

mymin <- function(...) {
  min <- Inf
  if (missing(...)) {
    warning("missing arguments; returning Inf")
  } else {
    for (i in c(...)) {
      if (i < min) {
        min <- i
      }
    }
  }
  return(min)
}

myfind <- function(name) {
  result <- NULL
  for (package in search()) {
    if (is.element(name, ls(name = package)))
      result <- c(result, package)
  }
  return(result)
}

myfind("pi")
myfind("rnorm")


# lapply

L <- list(a = 25:30, 
          b = matrix(1:12, 3, 4), 
          c = rnorm(100), 
          d = sample(10))
lapply(L, mean)

lapply(L, length)

lapply(2:4, runif)

lapply(2:4, runif, min = 0, max = 10)

lapply(datasets::faithful, max)

lapply(faithful, min)

lapply(faithful, function(x) {max(x) - min(x)})

# sapply

sapply(L, mean)

sapply(L, range)

lapply(faithful, range)

lapply(faithful, quantile)

sapply(faithful, quantile)

sapply(faithful, quantile, c(0, 1/3, 2/3, 1))

sapply(faithful, quantile, seq(0, 1, 0.1))

# apply

m <- matrix(sample(12), nrow = 3, ncol = 4); m

apply(m, 1, min)

apply(m, 2, max)

m <- matrix(sample(8))
dim(m) <- c(2, 2, 2); m

apply(m, 1, mean)

apply(m[ , , 1], 1, mean)

apply(m, 2, mean)

apply(m[ , , 2], 2, mean)

total <- sum(datasets::HairEyeColor); total

apply(HairEyeColor, 1, sum) / total
apply(HairEyeColor, 2, sum) / total
apply(HairEyeColor, 3, sum) / total

apply(HairEyeColor, "Hair", sum) / total
apply(HairEyeColor, "Eye", sum) / total
apply(HairEyeColor, "Sex", sum) / total

# mapply

mapply(rep, 1:3, 5)
mapply(rep, 5, 1:3)

mapply(log, 2:5, 2:3)

tipo1 <- sample(10:99, 10); tipo1
tipo2 <- sample(10:99, 10); tipo2
tipo3 <- sample(10:99, 10); tipo3
tipo4 <- sample(10:99, 10); tipo4

mapply(min, tipo1, tipo2, tipo3, tipo4)
mapply(max, tipo1, tipo2, tipo3, tipo4)
mapply(function (...) {mean(c(...))}, 
       tipo1, tipo2, tipo3, tipo4)

(tipo1 <- tipo1 >= 50)
(tipo2 <- tipo2 >= 50)
(tipo3 <- tipo3 >= 50)
(tipo4 <- tipo4 >= 50)

mapply(all, tipo1, tipo2, tipo3, tipo4)
mapply(any, tipo1, tipo2, tipo3, tipo4)

# tapply

x <- c(rnorm(100), runif(100), sample(100))
f <- gl(n = 3, k = 100, 
        labels = c("norm", "unif", "sample"))

df <- data.frame(x, f)
tapply(df$x, df$f, range)

df <- df[sample(nrow(df)), ]
tapply(df$x, df$f, range)
tapply(df$x, df$f, mean)

tapply(datasets::mtcars$mpg,
       datasets::mtcars$cyl, mean)
tapply(mtcars$qsec, mtcars$cyl, mean)
tapply(mtcars$hp, mtcars$vs, mean)

tapply(row.names(mtcars), mtcars$gear, c)
tapply(mtcars$hp, mtcars$gear, c)
tapply(mtcars$hp, mtcars$gear, length)

tapply(mtcars$hp, list(mtcars$gear, mtcars$cyl), mean)
tapply(mtcars$hp, list(mtcars$gear, mtcars$cyl), length)

qfactor <- function(vector) {
  q <- quantile(vector)
  result <- NULL
  for (i in vector)
    if (i <= q["25%"])
      result <- c(result, "q1")
  else if (i <= q["50%"])
    result <- c(result, "q2")
  else if (i <= q["75%"])
    result <- c(result, "q3")
  else result <- c(result, "q4")
  return(as.factor(result))
}

qfactor(mtcars$hp)

tapply(mtcars$mpg, qfactor(mtcars$hp), mean)
tapply(mtcars$mpg, qfactor(mtcars$qsec), max)
tapply(mtcars$hp, qfactor(mtcars$mpg), mean)

tapply(datasets::Loblolly$height,
       datasets::Loblolly$age, min)
tapply(Loblolly$height, Loblolly$age, mean)
tapply(Loblolly$height, Loblolly$age, max)

tapply(datasets::airquality$Temp,
       datasets::airquality$Month, mean)
tapply(airquality$Solar.R, airquality$Month, 
       mean, na.rm = TRUE)
tapply(airquality$Ozone, airquality$Month, 
       mean, na.rm = TRUE)
tapply(airquality$Temp, airquality$Wind > 10,
       mean)
tapply(airquality$Temp, 
       ifelse(airquality$Wind > 10, 
              "Windy", "Not Windy"), mean)

# Agrupando com dois critérios
tapply(airquality$Temp, 
       list(airquality$Month, 
            ifelse(airquality$Wind > 10, 
                   "Windy", "Not Windy")), 
       mean)
tapply(airquality$Temp, 
       list(airquality$Month, 
            ifelse(airquality$Wind > 10, 
                   "Windy", "Not Windy")), 
       length)

tapply(datasets::iris$Petal.Length,
       datasets::iris$Species, mean)
tapply(iris$Petal.Width, iris$Species, mean)
tapply(iris$Petal.Length / iris$Petal.Width, 
       iris$Species, mean)

tapply(iris$Petal.Length, iris$Species, summary)
simplify2array(tapply(iris$Petal.Length, 
                      iris$Species, summary))

tapply(esoph$ncases, esoph$agegp, sum) # casos com câncer
tapply(esoph$ncontrols, esoph$agegp, sum) # casos sem câncer

# Fração das pessoas com câncer por faixa de idade
tapply(esoph$ncases, esoph$agegp, sum) / 
  (tapply(esoph$ncases, esoph$agegp, sum) + 
     tapply(esoph$ncontrols, esoph$agegp, sum))

# Fração das pessoas com câncer na mostra em cada faixa de idade 
tapply(esoph$ncases, esoph$agegp, sum) / 
  sum(tapply(esoph$ncases, esoph$agegp, sum))

# Fração das pessoas com câncer por faixa de consumo de álcool
tapply(esoph$ncases, esoph$alcgp, sum) / 
  (tapply(esoph$ncases, esoph$alcgp, sum) + 
     tapply(esoph$ncontrols, esoph$alcgp, sum))

# Fração das pessoas com câncer na mostra em cada faixa de consumo de álcool
tapply(esoph$ncases, esoph$alcgp, sum) / 
  sum(tapply(esoph$ncases, esoph$alcgp, sum))

# Fração das pessoas com câncer por faixa de consumo de tabaco
tapply(esoph$ncases, esoph$tobgp, sum) / 
  (tapply(esoph$ncases, esoph$tobgp, sum) + 
     tapply(esoph$ncontrols, esoph$tobgp, sum))

# Fração das pessoas com câncer na mostra em cada faixa de consumo de tabaco
tapply(esoph$ncases, esoph$tobgp, sum) / 
  sum(tapply(esoph$ncases, esoph$tobgp, sum))
