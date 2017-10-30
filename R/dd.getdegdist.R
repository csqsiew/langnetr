#' Fit a power law distribution to the degree distribution of the language network.
#'
#' @param network The language network. Must be an igraph object.
#' @return Degree distribution of network.
#' @examples
#' network <- watts.strogatz.game(1, 5000, 1, 0.35, loops = FALSE, multiple = FALSE) # a fake network
#' distribution <- getdegdist(network)
#' fitpl(distribution) # fit a power law to the distribution
#' fitln(distribution) # fit a log normal distribution instead
#'

getdegdist <- function(network) {

  if(igraph::is_igraph(network) == F) {
    stop("Network must be an igraph object.")
  }

  net.degree <- as.data.frame(table(as.data.frame(igraph::degree(network))))
  net.degree <- net.degree$Freq
  return(net.degree)

}
