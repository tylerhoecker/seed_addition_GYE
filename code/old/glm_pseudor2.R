# https://www.queryxchange.com/q/20_183699/how-to-calculate-pseudo-r2-when-using-logistic-regression-on-aggregated-data-files/

Pseudo.R2=function(object){
  stopifnot(object$family$family == "binomial")
  object0 = update(object, ~ 1)
  wt <- object$prior.weights # length(wt)
  y = object$y # weighted
  ones = round(y*wt)
  zeros = wt-ones
  fv <- object$fitted.values   # length(fv)
  if (is.null(object$na.action)) fv0 <- object0$fitted.values else
    fv0 <- object0$fitted.values[-object$na.action] # object may have missing values
  resp <- cbind(ones, zeros)
  Y <- apply(resp, 1, function(x) {c(rep(1, x[1]), rep(0, x[2]))} )
  if (is.list(Y)) Y <- unlist(Y) else Y <- c(Y)
  # length(Y); sum(Y)
  fv.exp <- c(apply(cbind(fv, wt), 1, function(x) rep(x[1], x[2])))
  if (is.list(fv.exp)) fv.exp <- unlist(fv.exp) else fv.exp <- c(fv.exp)
  # length(fv.exp)
  fv0.exp <- c(apply(cbind(fv0, wt), 1, function(x) rep(x[1], x[2])))
  if (is.list(fv0.exp)) fv0.exp <- unlist(fv0.exp) else fv0.exp <- c(fv0.exp)
  (ll = sum(log(dbinom(x=Y,size=1,prob=fv.exp))))
  (ll0 = sum(log(dbinom(x=Y,size=1,prob=fv0.exp))))
  
  n <- length(Y)
  G2 <- -2 * (ll0 - ll)
  McFadden.R2 <- 1 - ll/ll0
  CoxSnell.R2 <- 1 - exp((2 * (ll0 - ll))/n) # Cox & Snell / Maximum likelihood pseudo r-squared
  r2ML.max <- 1 - exp(ll0 * 2/n)
  Nagelkerke.R2 <- CoxSnell.R2/r2ML.max  # Nagelkerke / Cragg & Uhler's pseudo r-squared
  
  out <- c(llh = ll, llhNull = ll0, G2 = G2, McFadden = McFadden.R2,
           r2ML = CoxSnell.R2, r2CU = Nagelkerke.R2)
  out
}
