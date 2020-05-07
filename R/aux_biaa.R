# Created by: aleix11alcacer
# Created on: 06/05/2020


intpm <- function(ri) {
  (t(ri) %*% ri)
}
fnorm <- function(m) {
  return(sum(apply(m, 2, intpm)))
}


init_coefs <- function(x, k, c, ...) {
    n <- nrow(x)
    m <- ncol(x)
    a <- matrix(runif(n * k), nrow = n, ncol = k)
    a <- a / apply(a, 1, sum)
    g <- matrix(runif(c * m), nrow = c, ncol = m)
    g <- g / apply(g, 2, sum)
    b <- matrix(runif(k * n), nrow = k, ncol = n)
    b <- b / apply(b, 2, sum)
    o <- matrix(runif(m * c), nrow = m, ncol = c)
    o <- o / apply(o, 1, sum)
    return(list(alphas = a, gammas = g, betas = b, omegas = o))
}


update_alphas <- function(coefs, c, d, e, ...) {
  #require(nnls)
  b <- t(e)
  A <- t(c %*% d)

  A <- rbind(A, rep(200, ncol(A)))
  b <- rbind(b, rep(200, ncol(b)))

  n <- ncol(b)
  for (j in 1:n) {
    coefs[j,] <- t(coef(nnls::nnls(A, b[,j])))
  }
  return(coefs)
}

update_betas <- update_alphas

update_gammas <- function(coefs, c, d, e, ...) {
  #require(nnls)
  b <- e
  A <- d %*% c

  A <- rbind(A, rep(200, ncol(A)))
  b <- rbind(b, rep(200, ncol(b)))

  n <- ncol(b)
  for (j in 1:n) {
    coefs[,j] <- coef(nnls::nnls(A, b[,j]))
  }
  return(coefs)
}

update_omegas <- update_gammas


ginv_zs <- function(alphas, x, gammas, ...) {
  #require(MASS)
  return(MASS::ginv(alphas) %*% x %*% MASS::ginv(gammas))
}
