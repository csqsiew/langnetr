#' Get uncertainty estimates for power law parameters fitted to the degree distribution of the language network.
#'
#' @param model A power law model already fitted to the raw data.
#' @return If getraw = F, returns the raw bootstrap output. Can be used for further analysis or visualization. If getraw = T, returns means and sds of the estimated parameters from the bootstrap.
#' @examples
#' getplsd(model.pl, getraw = F)
#' # xmin mean, xmin sd, alpha mean, alpha sd values will be listed
#' # these are the means and sds of the simulations, not the actual data
#' # if the fitted alpha fits falls within the bootstrap distribution, then that's good
#' bsresults <- getplsd(model.pl, getraw = T)
#' # returns the raw outputs of the bootstrap
#'

getplsd <- function(model, getraw = F) { # distribution must already be fitted by fitpl()

  set.seed(1)                                             # for replicability
  ncores <- parallel::detectCores()                       # detect number of cores in computer
  bs = poweRlaw::bootstrap(model, no_of_sims=1000, threads=ncores)  # run the bootstrapping process
  xmin <- mean(bs$bootstraps[,2], na.rm = T)                        # mean of xmin from simulations
  alpha <- mean(bs$bootstraps[,3], na.rm = T)                       # mean of alpha from simulations
  xminsd <- sd(bs$bootstraps[,2], na.rm = T)                        # sd of xmin from simulations
  alphasd <- sd(bs$bootstraps[,3], na.rm = T)                       # sd of alpha from simulations

  if (getraw == F) {                                     # if raw bootstrap output is not wanted,
    output <- round(c(xmin, xminsd, alpha, alphasd), 3)  # return the uncertainty statistics
    return(output)
  } else {                            # if raw bootstrap is wanted, (for plotting purposes)
    return(bs)                        # return the raw bootstrap output
  }

}
