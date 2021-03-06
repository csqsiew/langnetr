#' Converts a list of words to an igraph network object.
#'
#' @param wordlist A list of words. Must be a character vector.
#' @return An igraph object of the language network created from \code{wordlist}.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' plot(somewordsnet) #plots the graph

tolangnet <- function(wordlist) {

  if (is.character(wordlist) == FALSE) {    # check if wordlist is character class
    stop("Data is not of character class.") # if not, output an error message
  }

  if (is.vector(wordlist) == FALSE) { # check if wordlist is a vector
    stop("Data is not in a vector.") # if not, output an error message
  }

  data_e <- data.frame() # create an empty data frame to store edges
  data_h <- data.frame() # create an empty data frame to store hermits

  for (x in 1:length(wordlist)) { # for all words in the list

    y<-unlist(levenshtein.neighbors.number(wordlist[x],wordlist)[1]) # list of 1-hop neighbors of word x

    if (length(y) > 0) {              # if the word has at least one neighbor
      a <- as.data.frame(cbind(x, y)) # generate edges: word x-neighbor 1, word x-neighbor 2, and so on...
      data_e <- rbind(data_e, a)      # add edge to data frame
    }

    else {                        # if word does not have any neighbors it is a hermit
      data_h <- rbind(data_h, x)  # store in data frame to keep a record
    }

  }

  if (nrow(data_e) != 0) {                                            # if there are edges formed, create igraph object

    g <- igraph::graph_from_data_frame(data_e, directed = F, vertices = NULL) # note that the edgelist generated above has duplicate edges
    g <- igraph::simplify(g)                                                  # to remove duplicate edges

    if (nrow(data_h) != 0) {            # check for hermits
      g <- g + as.character(data_h[ ,1]) # add hermits to the graph, requires a hack to convert hermit IDs to characters to add as new vertices with same IDs
    }

    return(g) # returns the network as an igraph object

  } else {
    print("List of words given do not form a network. Might be hermits") # if the words do not form a network, output an error message
    }

}
