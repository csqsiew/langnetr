#' Visualize the fits of various distributions to the degree distribution of the language network.
#'
#' @param distribution A vector of integers representing the degree distribution of a given network.
#' @return A figure with the distribution fits to the raw data.
#' @examples
#' # visualize(distribution)
#'

visualize <- function(distribution) {

  test <- distribution

  # power law
  m_pl = displ$new(test)
  est = estimate_xmin(m_pl) # estimate lower threshold
  m_pl$setXmin(est) # update power law object

  # log normal
  m_ln = dislnorm$new(test)
  est = estimate_xmin(m_ln) # estimate lower threshold
  m_ln$setXmin(est) # update object

  # exponential
  m_ex = disexp$new(test)
  est = estimate_xmin(m_ex) # estimate lower threshold
  m_ex$setXmin(est) # update object

  # poisson
  m_po = dispois$new(test)
  est = estimate_xmin(m_po) # estimate lower threshold
  m_po$setXmin(est) # update object

  # view fits
  plot(m_pl,
       ylab = 'Cummulative distribution (CDF)',
       xlab = 'Log of degree counts (occurrence)',
       main = 'Degree distribution')
  lines(m_pl, col=2) # red = power law
  lines(m_ln, col=3) # green = log normal
  lines(m_po, col=4) # blue = poisson
  lines(m_ex, col=5) # teal = exponential

  legend("bottomleft",
         lty=c(1,1),
         legend = c("power law", "log normal", 'Poisson', 'exponential'),
         col = c(2:5))
}
