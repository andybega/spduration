# Define functions to perform Monte Carlo simulations ---------------------

## function to return EV
rt1ev <- function(x.uniform, shape = 1, scale = 1){
  x.t1ev <- log(qweibull(x.uniform, shape = shape, scale = scale))
  return(x.t1ev)
}

## function to generate data
mc.dgp <- function(
  p1 = 1, 
  b1 = c(1, 0), g = c(1, -1,0),
  rho.mc = 0, n=samp.size){
  
  ## Split
  cure <- rep(0, times=n)
  cure[g[1] * x13 + g[2] * x3 + rlogis(n,location=g[3]) > 0] <- 1
  
  ## Correlated errors
  vmat <- matrix(c(1, rho.mc, rho.mc, 1), nrow=2)
  if (rho.mc >= 0.8 | rho.mc <= -0.8) {vmat <- make.positive.definite(vmat)}
  v <- rmvnorm(n, c(0,0), vmat)
  u1 <- pnorm(v[,1]); u2 <- pnorm(v[,2])
  e1 <- rt1ev(u1, shape=p1)

  
  ## Duration
  y1 <- exp(b1[1] * x1 + b1[2] * x3 + (1/p1) * e1)
  

  
  ## Cured spells
  y1c <- y1
  y1c[cure==1] <- max(y1)

  rc <- rep(0, times= n); rc[cure==1] <- 1
  
  p1.data <- data.frame(y1=y1c, t0=0, rc=rc, fo=1, lo=1, 
                        x1=x1, x2=x2, x3=x3, x13=x13, spellID=1:n)
   out <- list(p1.data = p1.data)
  return(out)
}
