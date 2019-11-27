
# Wrappers for dgamma, pgamma, etc which use mean and sd as parameters.

getGammaPar <- function(mean, sd) {
  if(any(mean <= 0))
    stop("'mean' must be greater than zero.")
  if(any(sd <= 0))
    stop("'sd' must be greater than zero.")
  rate <- mean / sd^2
  shape <- rate * mean
  cbind(shape=shape, rate=rate)
}

dgamma2 <- function(x, mean, sd) {
  sr <- getGammaPar(mean, sd)
  return(dgamma(x, sr[,1], sr[,2]))
}

pgamma2 <- function(q, mean, sd, lower.tail=TRUE, log.p=FALSE) {
  sr <- getGammaPar(mean, sd)
  return(pgamma(q, sr[,1], sr[,2], lower.tail=lower.tail, log.p=log.p))
}
qgamma2 <- function(p, mean, sd, lower.tail=TRUE, log.p=FALSE) {
  sr <- getGammaPar(mean, sd)
  return(qgamma(p, sr[,1], sr[,2], lower.tail=lower.tail, log.p=log.p))
}

rgamma2 <- function(n, mean, sd) {
  sr <- getGammaPar(mean, sd)
  return(rgamma(n, sr[,1], sr[,2]))
}

