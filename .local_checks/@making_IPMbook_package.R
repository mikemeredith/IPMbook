
setwd("D:/Github/IPMbook_package") # my desktop
setwd("C:/Github/IPMbook_package") # my laptop
dir()

# Install dependencies
install.packages("abind")

library(spelling)
update_wordlist(pkg = "IPMbook", confirm = TRUE)
out <- spell_check_package(pkg = "IPMbook")

# For package development
system("R CMD INSTALL IPMbook") # Use this for a "dev" install.
devtools::load_all("IPMbook")


# Create the IPMbook package
# ==========================
unlink(list.files(pattern="Rplots.pdf", recursive=TRUE))
system("R CMD build IPMbook")  # Produces the .tar.gz
pkg <- "IPMbook_0.1.2.tar.gz"  # <-- fix version number here

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

example(marray)
example(marrayAge)
example(marrayDead)

data(bats)
ini <- zInit(bats$ch)
head(ini)
kn <- zKnown(bats$ch)
head(kn)

example(simPop)
example(simCapHist)
example(simProd)

example(demoMCMC)
