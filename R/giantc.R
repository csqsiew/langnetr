#' Extract list of nodes in the giant component of the language network.
#'
#' @param langnet An igraph object created using the \code{tolangnet} function and with label names created with the \code{nodeindex} function.
#' @return A list of nodes in the LCC of the language network.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' somewordsnet <- nodeindex(somewordsnet, somewords)
#' somewords.gc <- giantc(somewordsnet)
#' somewords.gc
#'
giantc <- function(langnet) {
  clu <- igraph::components(langnet)     # in-built function in igraph that labels all nodes with a component id
  y <- clu$membership                    # note that y is a named vector
  z <- sort(table(y), decreasing = T)[1] # sort by decreasing so that the component id with the most nodes (the gc) is the first element
  gc <- which(y == names(z))             # extract nodes in the largest connected component
  gc.names <- names(gc)
  return(gc.names)
}
