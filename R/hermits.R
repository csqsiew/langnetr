#' Extract list of hermits from an igraph network object.
#'
#' @param langnet An igraph object created using the \code{tolangnet} function and with label names created with the \code{nodeindex} function.
#' @return A list of hermits (nodes with no neighbors).
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' somewordsnet <- nodeindex(somewordsnet, somewords)
#' somewords.hermits <- hermits(somewordsnet)
#' hermits # to view hermits

hermits <- function(langnet) {
  x <- as.data.frame(igraph::degree(langnet)) # get a list of nodes and their degrees
  hermits <- rownames(x)[which(x == 0)]       # extract nodes if degree == 0
  return(hermits)                             # if no hermits, hermits() will return an empty vector
}
