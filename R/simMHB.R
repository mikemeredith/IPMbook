
# Chapter 4

# p 20 of MS dated 2020-11-13

# ----- Start of function definition -----------
simMHB <- function(nsites = 267, nsurveys = 3, nyears = 25,
    mean.lam = 1, mean.beta = 0.03, sd.lam = c(0.5, 0.05),
    mean.p = 0.6, beta.p = 0.1,
    show.plot = TRUE){
  #
  # Function simulates MHB lookalike data. The MHB is the Swiss breeding
  # bird survey that is described in many chapters in the AHM1 and
  # AHM2 books. This survey was launched in 1999 and for a total of
  # 267 1km2 quadrats laid out in an approximate grid over Switzerland,
  # 2 or 3 surveys are conducted in each breeding season (mid April to
  # early July) on a quadrat-specific, constant route averaging 4-6 km
  # and all birds detected are mapped, thus yielding replicated counts
  # of unmarked individuals.
  #
  # The data are simulated under the assumptions of a binomial
  # N-mixture model where for lambda we can specify a log-linear trend
  # over the years and we can account for site-level random effects in
  # both the intercept and the slopes of the log-linear model.
  # For detection probability we can currently a constant average or
  # a logit-linear trend over the years, with no further heterogeneity.
  #
  # Function arguments:
  # nsites: number of sites
  # nyears: number of years
  # nsurveys: number of replicate surveys per year
  # mean.lam: intercept of expected abundance
  # mean.beta: average slope of log(lambda) on year (centered)
  # sd.lam: SDs of the Normal distribution from which random site
  #    effects for the intercept and the slope in the log-linear
  #    model for lambda are drawn randomly
  # mean.p: value of constant detection probability per survey (or
  #    intercept of the logit-linear model for p)
  # beta.p: slope of the logit(p) in year(centered)

  # -------- check and fix input -------------------
  nsites <- round(nsites[1])
  stopifNegative(nsites, allowNA=FALSE, allowZero=FALSE)
  nsurveys <- round(nsurveys[1])
  stopifNegative(nsurveys, allowNA=FALSE, allowZero=FALSE)
  nyears <- round(nyears[1])
  stopifnotGreaterthan(nyears, 1, allowNA=FALSE)
  stopifNegative(mean.lam, allowNA=FALSE, allowZero=FALSE)
  stopifNegative(sd.lam, allowNA=FALSE, allowZero=TRUE)
  stopifnotProbability(mean.p, allowNA=FALSE)
  # -------------------------------------------------------

  year <- (1:nyears) - ceiling(nyears/2)  # Year covariate (centered)

  # Simulate true system state: Expected and realized abundance
  alpha <- rnorm(nsites, log(mean.lam), sd.lam[1])
  beta <- rnorm(nsites, mean.beta, sd.lam[2])
  lam <- N <- array(NA, dim = c(nsites, nyears))
  for(t in 1:nyears){
    lam[,t] <- exp(alpha + beta * year[t])
    N[,t] <- rpois(nsites, lam[,t])
  }

  # Simulate counts with binomial observation model
  C <- array(NA, dim = c(nsites, nsurveys, nyears))
  p <- plogis(qlogis(mean.p) + beta.p * year)
  for(i in 1:nsites){
    for(t in 1:nyears){
      C[i,,t] <- rbinom(nsurveys, N[i,t], p[t])
    }
  }
  totalN <- apply(N, 2, sum)   # Total annual abundance (all sites)

  # Plotting output (if desired)
  if(show.plot){
    op <- par(mfrow = c(2, 2), mar = c(5,5,5,2), cex.lab = 1.5,
        cex.axis = 1.5, cex.main = 1.5) ; on.exit(par(op))
    ylim <- range(lam, N)
    matplot(1:nyears, t(lam), type = 'l', lty = 1, lwd = 2,
        ylim = ylim, frame = FALSE, xlab = 'Year', ylab = 'lambda',
        main = 'Expected abundance')
    matplot(1:nyears, jitter(t(N)), type = 'l', lty = 1, lwd = 2,
        ylim = ylim, frame = FALSE, xlab = 'Year', ylab = 'N',
        main = 'Realized abundance')
    plot(1:nyears, p, type = 'l', lty = 1, lwd = 2,
        ylim = c(0,1), frame = FALSE, xlab = 'Year', ylab = 'p',
        main = 'Detection probability')
    matplot(1:nyears, jitter(t(apply(C, c(1,3), mean))), type = 'l',
        lty = 1, lwd = 2, ylim = ylim, frame = FALSE, xlab = 'Year',
         ylab = 'Average C',
         main = 'Observed counts\n(average per site and year)')
    #  plot(table(C), lend = 'butt', lwd = 10,
    # main = 'Frequency distribution \nof observed counts', frame = FALSE)
  }

  # Numerical output
  return(list(
    # ----- arguments input ------
    nsites = nsites, nsurveys = nsurveys, nyears = nyears, mean.lam = mean.lam,
    mean.beta = mean.beta, sd.lam = sd.lam, mean.p = mean.p, beta.p = beta.p,
    # ----- quantities generated -----------------
    alpha = alpha, beta = beta, lam = lam, N = N, totalN = totalN, p = p, C = C))
}
# ----- End of function definition -----------
