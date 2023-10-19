poisson_distribution <- function(x, lambda) {
  prob <- (lambda^x) * exp(-lambda) / factorial(x)
  return(prob)
}


x = 4
lambda = 3

result = poisson_distribution(x, lambda)
cat(paste("P(X =", x, ") =", result))
