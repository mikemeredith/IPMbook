
### Random walk MCMC for binomial proportion
############################################

# Code example adapted from p. 48 of Ntzoufras (2009)

# Random-walk sampler for a single parameter:
#   binomial data with y successes in N trials
#   estimate the probability of success on the logit scale, theta = logit(p)
#   uses a Normal prior for theta with mean mu.theta and SD s.theta
#   proposal values are drawn from a Normal distribution with mean = current value and
#     SD = prop.s
# niter = number of MCMC draws required
# init = starting value for theta

### Define function that does RW-MCMC
demoMCMC <- function(y = 20, N = 50, niter = 25000,
  mu.theta = 0, s.theta = 100, prop.s = 1, init = 0){

  # Initalize calculations
  start.time <- Sys.time()    # Set timer
  theta <- numeric(niter)     # Set up vector for posterior draws of theta
  acc.counter <- 0            # Initialize counter for acceptance
  current.theta <- init       # Initial value for theta

  # Start MCMC algorithm
  for (t in 1:niter){          # Repeat niter times
    prop.theta <- rnorm(1, current.theta, prop.s)    # Propose a value prop.theta

    # Calculate log(likelihood) and log(prior) and their log(product)
    #  for proposed and current values of theta
    # (We work with logs because likelihood can be tiny.)
    # log1p(x) = log(1+x) and is precise when x is very small.
    prop.llh <- prop.theta * y - N * log1p(exp(prop.theta))            # log(likelihood) for proposal
    prop.lp <- dnorm(prop.theta, mu.theta, s.theta, log = TRUE)        # log(prior) for proposal
    prop.log.product <- prop.llh + prop.lp                             # log(product) for proposal
    current.llh <- current.theta * y - N * log1p(exp(current.theta))   # log(likelihood) for current
    current.lp <- dnorm(current.theta, mu.theta, s.theta, log = TRUE)  # log(prior) for current
    current.log.product <- current.llh + current.lp                    # log(product) for current
    # Calculate the ratio of the products
    a <- exp(prop.log.product - current.log.product)

    # If a > 1, we accept the proposal; if a < 1, we accept with probability a.
    # To do that, compare a with a draw from a uniform {0,1} distribution
    u <- runif(1)
    if (u < a){                        # Always TRUE if a > 1
      current.theta <- prop.theta
      acc.counter <- acc.counter + 1   #  Counts the number of acceptances
    }
    #   browser()  # If unhashed, allows to inspect values of u and a at each iteration
    theta[t] <- current.theta
  }
  p <- plogis(theta)                 # Compute p from theta = logit(p)
  acc.prob <- acc.counter/niter        # Acceptance probability

  # Graphical output
  par(mfrow = c(2,2))     # Plots of theta=logit(p) and of p
  plot(theta, type = "l", ylab = "theta (=logit(p))")   # Plot 1: time-series plot of theta = logit(p)
  plot(p, type = "l", ylim = c(0,1))                    # Plot 2: time-series plot of p
  abline(h = y/N, col = "red")                          # Add maximum likelihood estimate
  abline(h = mean(p), col = "blue")                     # Add posterior mean
  hist(p, breaks = 100, col = "grey", main = "", freq = FALSE)  # plot 3: Histogram of posterior with smoothed line
  smooth <- density(p, adjust = 2)
  lines(smooth$x, smooth$y, type = 'l', lwd = 2, col = "blue")
  plot(acf(p, plot = FALSE), main = "", lwd = 3)        # Plot 4: Autocorrelation function plot

  # Display some summary information
  cat("\nAcceptance probability:", round(acc.prob, 2), "\n")
  end.time <- Sys.time()         # Stop time
  elapsed.time <- round(difftime(end.time, start.time, units='secs'), 2)  # Compute elapsed time
  cat(paste(niter, 'posterior values drawn in', elapsed.time, 'seconds\n\n')) # Output run time

  # Numerical output
  return(list(
      # -------- original arguments  ------------------
      y = y, N = N, mu.theta=mu.theta, s.theta = s.theta, prop.s = prop.s,
      # -------- generated values ---------------------
      theta = theta,        # MCMC chain for theta
      acc.prob = acc.prob)) # proportion of proposed values accepted

} # end of function

