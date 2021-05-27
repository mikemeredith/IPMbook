
### Random walk MCMC for binomial proportion
############################################

# Code example adapted from p. 48 of Ntzoufras (2009)

# Random-walk sampler for a single parameter:
#   binomial data with y successes in N trials
#   estimate the probability of success on the logit scale, ltheta = logit(theta)
#   uses a Normal prior for ltheta with mean mu.ltheta and SD sd.ltheta
#   proposal values are drawn from a Normal distribution with mean = current value and
#     SD = prop.sd
# niter = number of MCMC draws required
# init = starting value for theta

### Define function that does RW-MCMC
demoMCMC <- function(y = 20, N = 50, niter = 25000,
  mu.ltheta = 0, sd.ltheta = 100, prop.sd = 1, init = 0,
  quiet = FALSE, show.plots = TRUE){

  # Checks and fixes for input data -----------------------------
  y <- round(y)[1]
  stopifNegative(y, allowNA=FALSE, allowZero=TRUE)
  N <- round(N)[1]
  stopifNegative(N, allowNA=FALSE, allowZero=FALSE)
  if(N < y)
    stop("The value of 'y' cannot exceed 'N'", call.=FALSE)
  niter <- round(niter)[1]
  stopifNegative(niter, allowNA=FALSE, allowZero=FALSE)
  stopifNegative(sd.ltheta, allowNA=FALSE, allowZero=TRUE)
  stopifNegative(prop.sd, allowNA=FALSE, allowZero=TRUE)
  # --------------------------------------------------------------

  # Initialize calculations
  start.time <- Sys.time()    # Set timer
  ltheta <- numeric(niter)    # Set up vector for posterior draws of ltheta
  acc.counter <- 0            # Initialize counter for acceptance
  current.ltheta <- init      # Initial value for ltheta

  # Start MCMC algorithm
  for (t in 1:niter){          # Repeat niter times
    prop.ltheta <- rnorm(1, current.ltheta, prop.sd)    # Propose a value prop.ltheta

    # Calculate log(likelihood) and log(prior) and their log(product)
    #  for proposed and current values of ltheta
    # (We work with logs because likelihood can be tiny.)
    # log1p(x) = log(1+x) and is precise when x is very small.
    prop.llh <- prop.ltheta * y - N * log1p(exp(prop.ltheta))            # log(likelihood) for proposal
    prop.lprior <- dnorm(prop.ltheta, mu.ltheta, sd.ltheta, log = TRUE)   # log(prior) for proposal
    prop.log.product <- prop.llh + prop.lprior                           # log(product) for proposal
    current.llh <- current.ltheta * y - N * log1p(exp(current.ltheta))   # log(likelihood) for current
    current.lprior <- dnorm(current.ltheta, mu.ltheta, sd.ltheta, log = TRUE)  # log(prior) for current
    current.log.product <- current.llh + current.lprior                  # log(product) for current
    # Calculate the ratio of the products
    a <- exp(prop.log.product - current.log.product)

    # If a > 1, we accept the proposal; if a < 1, we accept with probability a.
    # To do that, compare a with a draw from a uniform {0,1} distribution
    u <- runif(1)
    if (u < a){                        # Always TRUE if a > 1
      current.ltheta <- prop.ltheta
      acc.counter <- acc.counter + 1   #  Counts the number of acceptances
    }
    #   browser()  # If unhashed, allows to inspect values of u and a at each iteration
    ltheta[t] <- current.ltheta
  }
  theta <- plogis(ltheta)              # Compute theta from ltheta = logit(theta)
  acc.prob <- acc.counter/niter        # Acceptance probability

  # Graphical output
  if(show.plots) {
    op <- par(mfrow = c(2,2)) ; on.exit(par(op))

    # Plot 1: time-series plot of ltheta = logit(theta)
    plot(ltheta, type = "l", ylab = "logit(theta)")

    # Plot 2: time-series plot of theta
    plot(theta, type = "l", ylim = c(0,1))
    abline(h = y/N, col = "red")                    # Add maximum likelihood estimate
    abline(h = mean(theta), col = "blue")           # Add posterior mean

    # plot 3: Histogram of posterior with smoothed line
    hist(theta, breaks = 100, col = "grey", main = "", freq = FALSE)
    lines(density(theta, adjust = 2), type = 'l', lwd = 2, col = "blue")

    # Plot 4: Autocorrelation function plot
    plot(acf(theta, plot = FALSE), main = "", lwd = 3, lend = "butt")
  }

  if(!quiet) {
    # Display some summary information
    cat("\nAcceptance probability:", round(acc.prob, 2), "\n")
    end.time <- Sys.time()         # Stop time
    elapsed.time <- round(difftime(end.time, start.time, units='secs'), 2)  # Compute elapsed time
    cat(paste(niter, 'posterior values drawn in', elapsed.time, 'seconds\n\n')) # Output run time
  }

  # Numerical output
  return(list(
      # -------- original arguments  ------------------
      y = y, N = N, mu.ltheta=mu.ltheta, sd.ltheta = sd.ltheta, prop.sd = prop.sd,
      # -------- generated values ---------------------
      ltheta = ltheta,      # MCMC chain for ltheta
      acc.prob = acc.prob)) # proportion of proposed values accepted

} # end of function

