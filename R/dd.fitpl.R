#' Fit a power law distribution to the degree distribution of the language network.
#'
#' @param distribution A vector of integers representing the degree distribution of a given network.
#' @return If estimates = F, returns the fitted model for subsequent analyses. If estimates = T, returns optimal xmin and parameters.
#' @examples
#' fitpl(distribution, estimates = T)
#' # xmin, alpha (par1) values will be listed
#' model.pl <- fitpl(distribution, estimates = F)
#' # use this model for other functions
#'

fitpl <- function(distribution, estimates = F) {
  requireNamespace("poweRlaw", quietly = TRUE)
  m_pl = poweRlaw::displ$new(distribution) # set up the distribution environment
  est = poweRlaw::estimate_xmin(m_pl)      # estimate the optimal xmin
  bestxmin <- est$xmin           # save the optimal xmin
  m_pl$setXmin(est)              # assign the optimal xmin to the model
  est = poweRlaw::estimate_pars(m_pl)      # estimate alpha parameter for a given xmin
  alpha <- est$pars              # save the alpha estimate

  if (estimates == T) {         # if estimates are wanted,
    output <- round(c(bestxmin, alpha), 3)
    return(output)              # return xmin and alpha
  } else {                      # if esimates are not wanted,
    return(m_pl)                # return the model object for subsequent analyses
  }

}
