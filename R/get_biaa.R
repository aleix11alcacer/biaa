# Created by: aleix11alcacer
# Created on: 06/05/2020


#' @title Get the archetypal model
#'
#' @description Get a specific archetypal model stored in a `"biaaGroup"` class list.
#'
#' @param data A `"biaaGroup"` class list.
#' @param k The number of archetypes.
#' @param c The number of variables.
#'
#' @examples 
#' getmodel(archetypes, 3, 5)
#'
#' @export
getmodel <- function(data, k, c) {
  att <- attributes(data)

  k = which(k %in% att$k)
  c = which(c %in% att$c)
  
  return(data[[k]][[c]])
}
