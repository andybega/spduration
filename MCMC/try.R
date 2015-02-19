samp.size <- 1000
set.seed(12345678)
vmat <- diag(5)
xmat <- rmvnorm(samp.size, rep(0,5), diag(5))
x1 <- pnorm(xmat[,1]) * 6 - 3
x2 <- pnorm(xmat[,2]) * 6 - 3
x3 <- pnorm(xmat[,3]) * 6 - 3
x13 <- pnorm(xmat[,4]) * 6 - 3

mc.data <- mc.dgp(  p1 = 1, 
  b1 = c(1, 0), g = c(1, -1,0),
  rho.mc = 0, n=samp.size)
  
  
  hist(mc.data[[1]]$y1)
  
  
  mc.res.n9 <- mc.est(n.sim=100); save(mc.res.n9, file="output/mcres.N9.rda")
