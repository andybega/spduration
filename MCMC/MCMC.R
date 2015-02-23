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
  
  p1.data <- data.frame(y1=y1c, t0=0, atrisk=ifelse(rc==1,0,1), failure=ifelse(rc==1,0,1), lo=1, 
                        x1=x1, x2=x2, x3=x3, x13=x13, spellID=1:n)
   out <- list(p1.data = p1.data)
  return(out)
}





## function to estimate models

mc.est <- function(
  n.sim = 100, seed = 123456, rho.mc = 0,
  p1 = 1, 
  b1 = c(1, 0), g = c(1,-1,0)){
  
  b.1.1 <- b.1.3  <- b.p1  <- matrix(NA, nrow=n.sim, ncol=2)
  g.1 <- g.3 <- matrix(NA, nrow=n.sim, ncol=2)
  rho <- matrix(NA, nrow=n.sim, ncol=2)
  pb <- txtProgressBar()
  set.seed(seed)
  for(i in 1:n.sim){
    setTxtProgressBar(pb,i/n.sim)
    mc.data <- mc.dgp(b1=b1, g=g, p1=p1, rho.mc=rho.mc)
    model <- spdur(y1 ~ x1+x3, atrisk ~ x3+x13, last="lo", t.0="t0",fail="failure", distr='weibull',data=mc.data[[1]])
    ## beta for x1 in Duration
    
    
    b.1.1[i,1] <- res $ NAIVE
    b.1.1[i,2] <- model[[1]][2]
    ## beta for x3 in Duration
    b.1.3[i,1] <- res $ NAIVE
    b.1.3[i,2] <- model[[1]][3]

    
    ## lnp1
    b.p1[i,1] <- res $ NAIVE
    b.p1[i,2] <- model[[1]][7]
   
    ## gamma for x3
    g.1[i,1] <- res $ NAIVE
    g.1[i,2] <- model[[1]][5]
    g.3[i,1] <- res $ NAIVE
    g.3[i,2] <- model[[1]][6]
    
  }
  out <- list(b.1.1 = b.1.1, b.1.3 = b.1.3,
              g.1 = g.1, g.3 = g.3, b.p1 = b.p1)
  return(out)
}

get.rmse <- function(vec, true){
  sqrt(mean((vec-true)^2))
}





