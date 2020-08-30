
setwd("D:/Github/IPMbook_package") # my desktop
setwd("C:/Github/IPMbook_package") # my laptop
dir()

# Install dependencies
# install.packages(c("plotrix", "raster", "RandomFields", "coda",
    # "unmarked", "mvtnorm", "spdep"))

# For package development
system("R CMD INSTALL IPMbook") # Use this for a "dev" install.
devtools::load_all("IPMbook")


# Create the IPMbook package
# ==========================
unlink(list.files(pattern="Rplots.pdf", recursive=TRUE))
system("R CMD build IPMbook")  # Produces the .tar.gz
pkg <- "IPMbook_0.0.0.9033.tar.gz"  # <-- fix version number here

# Pick one to check:
## on desktop
system(paste("R CMD check ", pkg))
system(paste("R CMD check ", pkg, "--as-cran"))  # as-cran now runs donttest
## on laptop
system(paste("R CMD check ", pkg, "--no-manual"))
system(paste("R CMD check ", pkg, "--as-cran --no-manual"))

# Pick one to install
system(paste("R CMD INSTALL ", pkg))            # install only
system(paste("R CMD INSTALL ", pkg, "--build")) # install and produce the .zip binary


# Try it out:
library(IPMbook)
?IPMbook
?abbreviated

data(bats)
ini <- zInit(bats$ch)
head(ini)
kn <- zKnown(bats$ch)
head(kn)

example(simPop)
example(simCapHist)
example(simProd)

example(demoMCMC)