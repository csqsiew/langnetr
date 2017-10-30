#' Compare different distributions for their fits to the data.
#'
#' @param m1 Fitted model on the left hand side of the function
#' @param m2 Fitted model on the right hand side of the function
#' @return If output = F, returns two-sided and one-side p-values. If output = T, returns the complete output of Vuong's test.
#' @examples
#' cpdist(model.pl, model.ln, output = F)
#' cpdist(model.pl, model.ex, output = F)
#' # see vignette for explanations on how to interpret the p-values of Vuong's test.

cpdist <- function(m1, m2, output = F) {

  # both distributions must have the same lower threshold for comparison
  m2$setXmin(m1$getXmin()) # set the second distribution object to have the same xmin as the first object
  est = poweRlaw::estimate_pars(m2)    # estimate the parameters for this particular value of xmin
  m2$setPars(est)

  comp = poweRlaw::compare_distributions(m1, m2)

  print(paste('The two-sided p-value is', round(comp$p_two_sided, 3)))
  print(paste('The one-sided p-value is', round(comp$p_one_sided, 3)))

  if (output == T) { # if the output of the test is wanted, return the results
    return(comp)
  }
}
