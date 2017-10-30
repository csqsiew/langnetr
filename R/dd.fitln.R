#' Fit a log normal distribution to the degree distribution of the language network.
#'
#' @param distribution A vector of integers representing the degree distribution of a given network.
#' @return If estimates = F, returns the fitted model for subsequent analyses. If estimates = T, returns optimal xmin and parameters.
#' @examples
#' fitln(distribution, estimates = T)
#' # xmin, par1, par2 values will be listed
#' model.ln <- fitln(distribution, estimates = F)
#' # use this model for other functions
#'

fitln <- function(distribution, estimates = F) {
  m_ln = poweRlaw::dislnorm$new(distribution) # set up the distribution environment
  est = poweRlaw::estimate_xmin(m_ln)         # estimate the optimal xmin
  bestxmin <- est$xmin              # save the optimal xmin
  m_ln$setXmin(est)                 # assign the optimal xmin to the model
  est = poweRlaw::estimate_pars(m_ln)         # estimate parameters for a given xmin
  parameters <- est$pars            # save the parameter estimates

  if (estimates == T) {         # if estimates are wanted,
    output <- round(c(bestxmin, parameters), 3)
    return(output)              # return xmin and alpha
  } else {                      # if esimates are not wanted,
    return(m_ln)                # return the model object for subsequent analyses
  }

}
