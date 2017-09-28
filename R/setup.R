# Minor changes to the levenshtein.distance.number function from vwr package and
# creating a levenshtein.neighbors.number function that calls the distance function.
# Required to provide each node with a unique ID and prevents the homophone problem.
# I.e., two words can have the same spelling but different phonology, or have the same
# phonology but different spelling. They are not the same word, but will be treated as
# one representation unless unique ID are given to each word.

levenshtein.distance.number <- function (xsource, targets)
{
  distances <- stringdist::stringdist(xsource, targets, method = "lv")
  names(distances) <- 1:length(targets) # orginal code from vwr library: names(distances) <- targets
  return(distances)
}

levenshtein.neighbors.number <- function (xsource, targets)
{
  results <- list()
  distances <- levenshtein.distance.number(xsource, targets)
  for (distance in min(distances):max(distances)) {
    results[distance] = list(names(which(distances == distance)))
  }
  return(results)
}
