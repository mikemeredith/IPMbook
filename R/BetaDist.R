
# Wrappers for dbeta, pbeta, etc which use mean and sd OR mode and concentration.

# Using mean and sd
# =================
getBeta2Par <- function(mean, sd) {
  stopifnotProbability(mean, allowNA=FALSE)
  stopifNegative(sd, allowNA=FALSE, allowZero=FALSE)

  nu <- mean * (1-mean) / sd^2 - 1
  if(any(nu <= 0)) {
    warning("sd is too large; some shape parameters will be NA.", call.=FALSE)
    nu[nu <= 0] <- NA
  }
  alpha <- mean * nu
  beta <- (1-mean) * nu
  cbind(shape1=alpha, shape2=beta)
}

dbeta2 <- function(x, mean, sd) {
  shapes <- getBeta2Par(mean,sd)
  return(dbeta(x, shapes[,1], shapes[,2]))
}

pbeta2 <- function(q, mean, sd, lower.tail=TRUE, log.p=FALSE) {
  shapes <- getBeta2Par(mean,sd)
  return(pbeta(q, shapes[,1], shapes[,2], lower.tail=lower.tail, log.p=log.p))
}
qbeta2 <- function(p, mean, sd, lower.tail=TRUE, log.p=FALSE) {
  shapes <- getBeta2Par(mean,sd)
  return(qbeta(p, shapes[,1], shapes[,2], lower.tail=lower.tail, log.p=log.p))
}

rbeta2 <- function(n, mean, sd) {
  shapes <- getBeta2Par(mean,sd)
  return(rbeta(n, shapes[,1], shapes[,2]))
}

# Using mode and concentration
# ============================
getBeta3Par <- function(mode, concentration) {
  stopifnotProbability(mode, allowNA=FALSE)
  if(any(concentration < 2))
    stop("'concentration' must be 2 or more.", call.=FALSE)
  alpha <- mode * (concentration - 2) + 1
  beta <- (1 - mode) * (concentration - 2) + 1
  cbind(shape1=alpha, shape2=beta)
}

dbeta3 <- function(x, mode, concentration) {
  shapes <- getBeta3Par(mode, concentration)
  return(dbeta(x, shapes[,1], shapes[,2]))
}

pbeta3 <- function(q, mode, concentration, lower.tail=TRUE, log.p=FALSE) {
  shapes <- getBeta3Par(mode, concentration)
  return(pbeta(q, shapes[,1], shapes[,2], lower.tail=lower.tail, log.p=log.p))
}
qbeta3 <- function(p, mode, concentration, lower.tail=TRUE, log.p=FALSE) {
  shapes <- getBeta3Par(mode, concentration)
  return(qbeta(p, shapes[,1], shapes[,2], lower.tail=lower.tail, log.p=log.p))
}

rbeta3 <- function(n, mode, concentration) {
  shapes <- getBeta3Par(mode, concentration)
  return(rbeta(n, shapes[,1], shapes[,2]))
}

