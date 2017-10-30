#' Test the fit of a power law to the degree distribution of the language network.
#'
#' @param model A power law model already fitted to the raw data.
#' @return If getraw = F, returns the raw bootstrap output. Can be used for further analysis or visualization. If getraw = T, returns means and sds of the estimated parameters from the bootstrap.
#' @examples
#' testpl(model.pl, getraw = F)
#' # xmin mean, xmin sd, alpha mean, alpha sd, p value will be listed
#' # interpreting the p-value: if p value is not sig, cannot reject H0
#' # i.e., we cannot rule out the power law model
#' # H0: data is generated from a power law distribution.
#' # H1: data is not generated from a power law distribution.
#' bspresults <- testpl(model.pl, getraw = T)
#' # returns the raw outputs of the bootstrap
#'

testpl <- function(model, getraw = F) { # distribution must already be fitted by fitpl()

  requireNamespace("poweRlaw", quietly = TRUE)

  set.seed(1)                                                # for replicability
  ncores <- parallel::detectCores()                          # number of cores in computer
  bs_p = poweRlaw::bootstrap_p(model, no_of_sims=1000, threads=ncores) # run the bootstrapping process
  xmin <- mean(bs_p$bootstraps[,2], na.rm = T)                         # mean of xmin from simulations
  alpha <- mean(bs_p$bootstraps[,3], na.rm = T)                        # mean of alpha from simulations
  xminsd <- sd(bs_p$bootstraps[,2], na.rm = T)                         # sd of xmin from simulations
  alphasd <- sd(bs_p$bootstraps[,3], na.rm = T)                        # sd of alpha simulations
  pvalue <- bs_p$p                                           # p-value for the power law test

  if (getraw == F) { # if raw bootstrap output is not wanted,
    output <- round(c(xmin, xminsd, alpha, alphasd, pvalue), 3) # return the uncertainty statistics
    return(output)
  } else { # if raw bootstrap is wanted, (for plotting purposes)
    return(bs_p) # return the raw bootstrap output
  }

}
