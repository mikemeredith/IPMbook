
# Some crude checks for the simulation functions in AHMbook package

# This checks both old and new (> 3.6.0) sample algorithms

# It uses the new improved version of checkTotal
checkTotal <- function(x, target=NA, digits=3, halt=FALSE) {
  addItUp <- function(x) {
    if(is.list(x))
      x <- sapply(x, addItUp)
    x1 <- suppressWarnings(try(as.numeric(x), silent=TRUE))
    if(inherits(x1, "try-error"))
      return(0)
    x2 <- mean(x1[is.finite(x1)], na.rm=TRUE)
  }
  added <- addItUp(x)
  if(is.na(target))
    return(added)
  ok <- isTRUE(all.equal(round(target, digits), round(added, digits)))
  if(halt && !ok)
    stop("Check failed for '", deparse(substitute(x)), "'", call.=FALSE)
  ok
}


library(IPMbook)

# nreps <- (getRversion() >= "3.6.0") + 1
# sample.kind <- c("Rounding", "Rejection") # no need to check with old RNG

RNGkind("Mersenne-Twister", "Inversion")

# halt <- FALSE
halt <- TRUE
  # If TRUE, the script will stop if output is not correct.

  # Chapter 4
  # ---------

  # simMHB
  # ''''''
    set.seed(123)
  # checkTotal(simMHB(show.plot=FALSE))
  checkTotal(simMHB(show.plot=FALSE), 41.85482, 4, halt)

  # Chapter 5
  # ---------

  # simPop
  # ''''''
  set.seed(123)
  # checkTotal(simPop())
  checkTotal(pop <- simPop(), 7.312845, 5, halt)

  # simCapHist
  # ''''''''''
  set.seed(123)
  # checkTotal(simCapHist(pop$state))
  # checkTotal(simCapHist(pop$state), 0.599819, 5, halt)
  checkTotal(simCapHist(pop$state, verbose = FALSE), 0.599819, 5, halt)

  # simCountBin
  # '''''''''''
  set.seed(123)
  # checkTotal(simCountBin(pop$totBreeders, p=0.6))
  checkTotal(simCountBin(pop$totBreeders, p=0.6), 6.133333, 5, halt)

  # simCountNorm
  # ''''''''''''
  set.seed(123)
  # checkTotal(simCountNorm(pop$totBreeders, sigma=10))
  checkTotal(simCountNorm(pop$totBreeders, sigma=10), 17.33333, 5, halt)

  # simCountPois
  # ''''''''''''
  set.seed(123)
  # checkTotal(simCountPois(pop$totBreeders))
  checkTotal(simCountPois(pop$totBreeders), 20.5, 5, halt)

  # simProd
  # '''''''
    set.seed(123)
  # checkTotal(simProd(pop$reprod))
  # checkTotal(simProd(pop$reprod), 4.304167, 4, halt)
  checkTotal(simProd(pop$reprod, verbose=FALSE), 4.304167, 4, halt)


if(halt) cat("\n\n*** Yay! No problems! ***\n\n")
