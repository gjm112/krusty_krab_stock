#mu: Drift
#sigma_sq: volatilty
#S_0: initial price of the stock
#t: how many days we want to simulate

S <- function(mu = .05, sigma_sq = 0.0025, S_0 = 100, t = 10){
  out <- rep(NA, t)
  
  Z <- rnorm(1)
  out[1] <- S_0 * exp(mu - 1/2*sigma_sq + sqrt(sigma_sq)*Z)
  
  for (i in 2:t){
    Z <- rnorm(1)
    out[i] <- out[i-1] * exp(mu - 1/2*sigma_sq + sqrt(sigma_sq)*Z)
  }
  
  return(out)

  
}