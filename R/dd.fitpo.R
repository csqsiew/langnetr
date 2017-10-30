#' Fit a Poisson distribution to the degree distribution of the language network.
#'
#' @param distribution A vector of integers representing the degree distribution of a given network.
#' @return If estimates = F, returns the fitted model for subsequent analyses. If estimates = T, returns optimal xmin and parameters.
#' @examples
#' fitpo(distribution, estimates = T)
#' # xmin, par1 values will be listed
#' model.po <- fitln(distribution, estimates = F)
#' # use this model for other functions
#'

fitpo <- function(distribution, estimates = F) {
  requireNamespace("poweRlaw", quietly = TRUE)
  m_po = poweRlaw::dispois$new(distribution) # set up the distribution environment
  est = poweRlaw::estimate_xmin(m_po)       # estimate the optimal xmin
  bestxmin <- est$xmin            # save the optimal xmin
  m_po$setXmin(est)               # assign the optimal xmin to the model
  est = poweRlaw::estimate_pars(m_po)       # estimate parameters for a given xmin
  parameters <- est$pars          # save the parameter estimates

  if (estimates == T) {         # if estimates are wanted,
    output <- round(c(bestxmin, parameters), 3)
    return(output)              # return xmin and alpha
  } else {                      # if esimates are not wanted,
    return(m_po)                # return the model object for subsequent analyses
  }

}
