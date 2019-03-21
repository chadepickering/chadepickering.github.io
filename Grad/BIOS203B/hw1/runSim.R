gaussian = "gaussian"
t1 = "t1"
t5 = "t5"

## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text = arg))
}

## check if a given integer is prime
isPrime <- function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes <- function(x) {
  n <- length(x)
  ind <- sapply(1:n, isPrime)
  return (mean(x[ind]))
}

avgMSE <- function(seed, n, dist, rep){
  set.seed(seed)
  prime_vector <- c()
  classical_vector <- c()
  
  for(i in 1:rep){
    if (dist == gaussian){
      x <- rnorm(n)
    } else if (dist == t1){ # Cauchy distribution has no mean!
      x <- rt(n, 1)
    } else if (dist == t5){
      x <- rt(n, 5)
    }
    
    prime_vector[i] <- (estMeanPrimes(x) - 0)^2
    classical_vector[i] <- (mean(x) - 0)^2
  }
  
  prime_mse <- sum(prime_vector)/rep
  classical_mse <- sum(classical_vector)/rep
  return(c(prime_mse, classical_mse))
}

avgMSE(seed, n, dist, rep)