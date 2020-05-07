# Created by: aleix11alcacer
# Created on: 06/05/2020

#'@include step_biaa.R


#' @title Bi-archetypes
#'
#' @description Perform bi-archetypal analysis on a dataset in order to obtain the data archetypes.
#'
#' @param data A \eqn{n \times m} matrix where \eqn{n} is the number of observations and \eqn{m}
#' is the number of variables.
#' @param k A list with the desired numbers of archertypes.
#' @param c A list with the desired numbers of variables.
#' @param nrep The number of repetitions that each scenario is computed.
#' @param maxiter The maximum number of iterations.
#' @param minimpr The minimal value of improvement between two iterations.
#' @return A matrix with all possible scenarios (determined by the Cartesian product between
#' `k` and` c`).
#'
#' @export
biaa <- function(data, k, c, nrep = 10, maxiter = 10000, minimpr = sqrt(.Machine$double.eps)) {
  # Create an array to store the archetypes to be calculated
  archetypes <- list()
  for (i in seq_along(k)) {
    archetypes[[i]] <- list()
  }
  # Compute the archetypes for a specific k and specific c
  for (i in seq_along(k)) {
    for (j in seq_along(c)) {
      archetypes[[i]][[j]] <- step_biaa(data, k[i], c[j], nrep, maxiter, minimpr);
    }
  }
  # Return the array of archetypes
  return(archetypes)
}
