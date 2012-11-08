\name{pred_loglog}
\alias{pred_loglog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict fitted values for loglog model
}
\description{
For internal use only.
}
\usage{
pred_loglog(coef, vcv, Y, X, Z, stat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coef}{
%%     ~~Describe \code{y} here~~
}
  \item{vcv}{
%%     ~~Describe \code{y} here~~
}
  \item{Y}{
%%     ~~Describe \code{y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{stat}{
%%     ~~Describe \code{sims} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, X, Z, sims) 
{
    require(MASS)
    coeff.b <- coef[1:ncol(X)]
    coeff.g <- coef[(ncol(X) + 1):(ncol(X) + ncol(Z))]
    coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
    al.hat <- exp(-coeff.a)
    la.hat <- exp(-X \%*\% coeff.b)
    n.cure <- plogis(Z \%*\% coeff.g)
    cure <- 1 - n.cure
    st <- exp(-(la.hat * Y[, 2])^al.hat)
    s0 <- exp(-(la.hat * (Y[, 2] - 1))^al.hat)
    cure.t <- cure/(st + cure * (1 - st))
    n.cure.t <- 1 - cure.t.in
    if (distr == "weibull") {
        ft.in <- la.hat.in * al.hat * (la.hat.in * Y[, 2])^(al.hat - 
            1) * exp(-(la.hat.in * Y[, 2])^al.hat)
        ft.out <- la.hat.out * al.hat * (la.hat.out * Y.test[, 
            2])^(al.hat - 1) * exp(-(la.hat.out * Y.test[, 2])^al.hat)
        ft.pred <- la.hat.pred * al.hat * (la.hat.pred * Y.pred[, 
            2])^(al.hat - 1) * exp(-(la.hat.pred * Y.pred[, 2])^al.hat)
        ft.pred.2 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 1))^(al.hat - 1) * exp(-(la.hat.pred * (Y.pred[, 
            2] + 1))^al.hat)
        ft.pred.3 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 2))^(al.hat - 1) * exp(-(la.hat.pred * (Y.pred[, 
            2] + 2))^al.hat)
        ft.pred.4 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 3))^(al.hat - 1) * exp(-(la.hat.pred * (Y.pred[, 
            2] + 3))^al.hat)
        ft.pred.5 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 4))^(al.hat - 1) * exp(-(la.hat.pred * (Y.pred[, 
            2] + 4))^al.hat)
        ft.pred.6 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 5))^(al.hat - 1) * exp(-(la.hat.pred * (Y.pred[, 
            2] + 5))^al.hat)
    }
    if (distr == "loglog") {
        ft.in <- (la.hat.in * al.hat * (la.hat.in * Y[, 2])^(al.hat - 
            1))/((1 + (la.hat.in * Y[, 2])^al.hat)^2)
        ft.out <- (la.hat.out * al.hat * (la.hat.out * Y.test[, 
            2])^(al.hat - 1))/((1 + (la.hat.out * Y.test[, 2])^al.hat)^2)
        ft.pred <- (la.hat.pred * al.hat * (la.hat.pred * Y.pred[, 
            2])^(al.hat - 1))/((1 + (la.hat.pred * Y.pred[, 2])^al.hat)^2)
        ft.pred.2 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 1))^(al.hat - 1))/((1 + (la.hat.pred * (Y.pred[, 
            2] + 1))^al.hat)^2)
        ft.pred.3 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 2))^(al.hat - 1))/((1 + (la.hat.pred * (Y.pred[, 
            2] + 2))^al.hat)^2)
        ft.pred.4 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 3))^(al.hat - 1))/((1 + (la.hat.pred * (Y.pred[, 
            2] + 3))^al.hat)^2)
        ft.pred.5 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 4))^(al.hat - 1))/((1 + (la.hat.pred * (Y.pred[, 
            2] + 4))^al.hat)^2)
        ft.pred.6 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[, 
            2] + 5))^(al.hat - 1))/((1 + (la.hat.pred * (Y.pred[, 
            2] + 5))^al.hat)^2)
    }
    pr.c.f.in <- n.cure.t.in * ft.in/s0.in
    pr.c.h.in <- n.cure.t.in * ft.in/st.in
    pr.u.f.in <- n.cure.in * ft.in/s0.in
    pr.u.h.in <- n.cure.in * ft.in/st.in
    pr.c.f.out <- n.cure.t.out * ft.out/s0.out
    pr.c.h.out <- n.cure.t.out * ft.out/st.out
    pr.u.f.out <- n.cure.out * ft.out/s0.out
    pr.u.h.out <- n.cure.out * ft.out/st.out
    pr.u.f.pred.1 <- n.cure.t.pred.1 * ft.pred/s0.pred
    pr.u.f.pred.2 <- n.cure.t.pred.2 * ft.pred.2/st.pred
    pr.u.f.pred.3 <- n.cure.t.pred.3 * ft.pred.3/st.pred.2
    pr.u.f.pred.4 <- n.cure.t.pred.4 * ft.pred.4/st.pred.3
    pr.u.f.pred.5 <- n.cure.t.pred.5 * ft.pred.5/st.pred.4
    pr.u.f.pred.6 <- n.cure.t.pred.6 * ft.pred.6/st.pred.5
    pr.in <- list(n.cure.t.in = n.cure.t.in, n.cure.in = n.cure.in, 
        pr.c.h.in = pr.c.h.in)
    pr.out <- list(n.cure.t.out = n.cure.t.out, n.cure.out = n.cure.out, 
        pr.c.h.out = pr.c.h.out)
    pred.tbl.ncure <- data.frame(month.1 = n.cure.t.pred.1, month.2 = n.cure.t.pred.2, 
        month.3 = n.cure.t.pred.3, month.4 = n.cure.t.pred.4, 
        month.5 = n.cure.t.pred.5, month.6 = n.cure.t.pred.6)
    pred.tbl.fail <- data.frame(month.1 = pr.u.f.pred.1, month.2 = pr.u.f.pred.2, 
        month.3 = pr.u.f.pred.3, month.4 = pr.u.f.pred.4, month.5 = pr.u.f.pred.5, 
        month.6 = pr.u.f.pred.6)
    rownames(pred.tbl.ncure) <- rownames(pred.tbl.fail) <- as.character(pred.data$country)
    output <- list(pr.in = pr.in, pr.out = pr.out, pred.tbl.ncure = pred.tbl.ncure, 
        pred.tbl.fail = pred.tbl.fail)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
