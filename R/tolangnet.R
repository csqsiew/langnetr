rm(list=ls())
gc(reset = TRUE)

#' Converts a list of words to an igraph network object.
#'
#' @param wordlist A list of words. Must be a character vector.
#' @return An igraph object of the language network created from \code{wordlist}.
#' @examples
#' somewords <- c('cat', 'bat', 'cap', 'cape')
#' somewordsnet <- tolangnet(somewords)
#' plot(somewordsnet) #plots the graph

sanity_check <- function(m1, m2) {

  m1[,1] <- as.character(m1[,1])
  m1[,2] <- as.character(m1[,2])

  m2[,1] <- as.character(m2[,1])
  m2[,2] <- as.character(m2[,2])

  m1 = (as.matrix(m1))
  m2 = (as.matrix(m2))


  m1 = as.data.frame(m1)
  m2 = as.data.frame(m2)
  colnames(m2) <- c("x", "y")

  setDT(m1)
  setDT(m2)
  m1 <- unique(m1)
  m2 <- unique(m2)

  setorder(m1, x, y)
  setorder(m2, x, y)

  row.names(m2) = 1:nrow(m2)
  row.names(m1) = 1:nrow(m1)

  setdiff(m1[,2], m2[,2])

  return(identical(m1, m2))
}

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

  return(data_e)
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

tolangnet_apply <- function(wordlist) {

  if (is.character(wordlist) == FALSE) {    # check if wordlist is character class
    stop("Data is not of character class.") # if not, output an error message
  }

  if (is.vector(wordlist) == FALSE) { # check if wordlist is a vector
    stop("Data is not in a vector.") # if not, output an error message
  }


  wrapper <- function(x, wordlist) {
    y<-unlist(levenshtein.neighbors.number(x,wordlist)[1]) # list of 1-hop neighbors of word x
    x1 = which(x == wordlist)
    if(length(y)==0) {y = 0}
    cbind(x1, y)
  }

  data_e = (lapply(wordlist, wrapper, wordlist))
  data_e = do.call(rbind, data_e)


  return(data_e)

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

tolangnet_parallel <- function(x, wordist) {


  if (is.character(wordlist) == FALSE) {    # check if wordlist is character class
    stop("Data is not of character class.") # if not, output an error message
  }

  if (is.vector(wordlist) == FALSE) { # check if wordlist is a vector
    stop("Data is not in a vector.") # if not, output an error message
  }

  wrapper <- function(x, wordlist) {

    y<-unlist(levenshtein.neighbors.number(x,wordlist)[1]) # list of 1-hop neighbors of word x
    x1 = which(x == wordlist)
    if(length(y)==0) {y = 0}
    cbind(x1, y)
  }

  data_e = (lapply(x, wrapper, wordlist))
  data_e = do.call(rbind, data_e)

  return(data_e)
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




###########################################################################
setwd("/home/jmartinez/cynthia_langnetr/langnetr/R")
library(stringdist)
library(igraph)
library(data.table)
library(doParallel)
source('setup.R')

#-----------------------------------------------------
# Generate a bunch of fake words.......
#-----------------------------------------------------
set.seed(1)

# Approximate number of words to generate. We use unique to
# make sure that there are no duplicates. You will begin
# to see improvement in performance with the paralleized
# function when N > 10000.
N = 20000

genwords <- function() {
  n = rpois(1, 5)
  paste(sample(letters, n, replace = TRUE), collapse="")
}
wordlist <- replicate(N, genwords())
wordlist <- unique(wordlist)



#-----------------------------------------------------
# Start the benchmarking.....
#-----------------------------------------------------
t1 = system.time({
  somewordsnet1 <- tolangnet(wordlist);
})

t2 = system.time({
    somewordsnet2 <- tolangnet_apply(wordlist);
    somewordsnet2[,2] <- as.numeric(somewordsnet2[,2])
    somewordsnet2[,1] <- as.numeric(somewordsnet2[,1])
    somewordsnet2 = somewordsnet2[which(somewordsnet2[,2] > 0),]
  })

#-----------------------------------------------------
# Parallelized version of code
#-----------------------------------------------------
# Think of the problem this way. Your computer has a number of
# clusters (logical processors). For each cluster, you are
# creating a new R instance, which means that you have to
# instantiate the R libraries you need and pass over the data
# and functions that you will be using for each instance.
#
# The code below, assumes you are working in the global environment.
# There is a way to wrap this into a function......
#

t3 = system.time({

  # Figure out how many cores are on the computer.
  nclus = detectCores()

  # Tell R how many clusters to make.
  clus = makeCluster(nclus)

  # Create a vector of same lenght of wordlist. We will use the
  # modulus operator to divide the data into distinct chunks.
  # Each chunk will be sent to the individual processor.
  f = 1:length(wordlist)
  f = f %% nclus

  # These varaibles and functions need to get exported to each cluster.
  clusterExport(clus,
                varlist = c("f",
                            "tolangnet_parallel",
                            "levenshtein.distance.number",
                            "levenshtein.neighbors.number",
                            "wordlist",
                            "nclus"
                            ),
                envir=environment()
  )


  # Par apply will send (1) chunks of data to be sent for
  # distance finding and (2) the entire wordlist.
  r <- parLapply(
    clus,             # Cluster Object.
    0:(nclus - 1),    # Cluster Number.
    function(i) {
      wd <- wordlist[f ==i]
      return(tolangnet_parallel(wd, wordlist))
    }
  )

  # Must stop the cluster.
  stopCluster(clus)

  # r is a list. do.call will collapse the entire list into a matrix.
  somewordsnet3 <- do.call(rbind, r)

  # so some cleaning up of the data.
  somewordsnet3[,2] <- as.numeric(somewordsnet3[,2])
  somewordsnet3 = somewordsnet3[which(somewordsnet3[,2] > 0),]

})

t1
t2
t3

# check to make sure that the results are generally the same. some
# manipulation to the data is done in here. Arguably, we don't have
# to create a function to do this check........
sanity_check(somewordsnet1, somewordsnet2)
sanity_check(somewordsnet1, somewordsnet3)

