#' Converts an igraph network object to an edgelist with labels.
#'
#' @param langnet An igraph object created using the \code{tolangnet} function and with label names created with the \code{nodeindex} function.
#' @return A dataframe of the *labeled* edgelist of the language network created from \code{wordlist}.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' somewordsnet <- nodeindex(somewordsnet, somewords)
#' somewords.edgelist <- toedgelist(somewordsnet)
#' head(somewords.edgelist)

toedgelist <- function(langnet) {
  el <- igraph::as_edgelist(langnet)
  el.names <- data.frame(word1 = igraph::V(langnet)[el[,1]]$name, #replace the node id with its name
                         word2 = igraph::V(langnet)[el[,2]]$name)
  return(el.names)
}
