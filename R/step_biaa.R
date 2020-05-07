# Created by: aleix11alcacer
# Created on: 06/05/2020

#'@include rep_biaa.R


step_biaa <- function (data, k, c, nrep, maxiter, minimpr) {
  best_archetypes <- NULL
  best_rss <- Inf

  # Calculate the archetypes nrep times and choose the archetypes that have best rss
  for (i in 1:nrep) {
    archetypes <- rep_biaa(data, k, c, maxiter, minimpr)
    if (archetypes$rss < best_rss) {
      best_archetypes <- archetypes
    }
  }
  return(best_archetypes)
}
