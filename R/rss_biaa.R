# Created by: aleix11alcacer
# Created on: 06/05/2020


#' @title Plot the bi-archetypal analysis RSS
#'
#' @description Plot in a the RSS of the all archetypes stored in a `"biaaGroup"` class list.
#'
#' @param data A `"biaaGroup"` class list.
#' @param phi A number giving the azimuthal direction. 
#' @param theta A number giving the colatitude.
#'
#' @examples 
#' rssplot(archetypes)
#' rssplot(archetypes, phi = 45, theta = 165)
#'
#' @export
rssplot <- function(data, phi = 30, theta = 135) {
  require("plot3D")
  att <- attributes(data)
  k <- att$k
  c <- att$c
  
  x <- matrix(0, nrow = length(k), ncol = length(c))
  for (j in seq_along(c)) {
    x[,j] = k
  }
  y <-  matrix(0, nrow = length(k), ncol = length(c))
  for (i in seq_along(k)) {
    y[i,] = c
  }
  z <-  matrix(0, nrow = length(k), ncol = length(c))
  labels <-  matrix(0, nrow = length(k), ncol = length(c))
  for (i in seq_along(k)) {
    for (j in seq_along(c)) {
      arch <- data[[i]][[j]]
      z[i, j] <- arch$rss
      labels[i, j] <- paste(arch$k, arch$c, sep = " - ")
    }
  }
  
  plot3D::scatter3D(x, y, z, phi = phi, theta = theta, pch=".", cex=10, col=gray.colors(50, rev = TRUE), xlab = "K", ylab = "C", zlab = "RSS")
  plot3D::text3D(x, y, z, labels, phi = phi, theta = theta, adj = 1.5, cex = 0.6, add = TRUE)
  plot3D::surf3D(x, y, z, phi = phi, theta = theta, facets=NA, border="black", lwd=0.1, colkey = FALSE, add = TRUE)
}
