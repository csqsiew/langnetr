# The nodeindex function maps the node numbers/IDs to the correct labels.
# Specifically, relabels the 'name' attribute in the igraph object.
# Note that the IDs are dependent on the ordering specified in the initial wordlist.

nodeindex <- function(langnet, wordlist) { # network created from the wordlist

  oldnames <- as.data.frame(as.numeric(igraph::V(langnet)$name)) # set up the node ids from the language network
  colnames(oldnames) <- 'nodeid'

  newnames <- as.data.frame(wordlist) # set up the wordlist for merging
  colnames(newnames) <- 'label'
  newnames$nodeid <- c(1:nrow(newnames))

  oldnames <- dplyr::left_join(oldnames, newnames, by = 'nodeid') # merge node ids and labels

  igraph::V(langnet)$name <- as.character(oldnames$label) # replace old names (ids) with new names (labels)

  return(langnet) #returns the igraph network with updated labels
}

# You can output to a .net file for further analysis, in gephi or pajek
# write.graph(test_graph, file='test_graph.net', format='pajek')
# BUT doesn't perserve labels - even node ids get completely shuffled
# Have to figure out a more robust way of export graphs.

