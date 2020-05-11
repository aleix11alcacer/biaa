# Created by: aleix11alcacer
# Created on: 06/05/2020

#'@include aux_biaa.R


rep_biaa <- function(data, k, c, maxiter, minimpr) {
  x <- data
  n <- nrow(x)

  # Init coefficients
  init <- init_coefs(data, k, c)
  alphas <- init$alphas
  gammas <- init$gammas
  betas <- init$betas
  omegas <- init$omegas

  # Compute the archetypes
  zbs <- betas %*% x %*% omegas

  # Obtain the rss
  resid <- alphas %*% zbs %*% gammas - x
  rss <- fnorm(resid) / n

  itr <- 1
  imp <- Inf

  while ((itr <= maxiter) & (imp > minimpr)) {
    # Calculate alphas fixing gammas
    alphas <- update_alphas(alphas, zbs, gammas, x)
    # Calculate gammas fixing alphas
    gammas <- update_gammas(gammas, zbs, alphas, x)

    # Compute the archetypes using alphas and gammas
    zas <- ginv_zs(alphas, x, gammas)

    # Calculate betas fixing omegas
    betas <- update_betas(betas, x, omegas, zas)
    #Calculate omegas fixing betas
    omegas <- update_omegas(omegas, x, betas, zas)

    # Compute the archetypes using betas and omegas
    zbs <- betas %*% x %*% omegas

    # Update alphas and gammas to calculate the rss
    alphas0 <- update_alphas(alphas, zbs, gammas, x)
    gammas0 <- update_gammas(gammas, zbs, alphas, x)

    # Compute the rss
    resid <- alphas0 %*% zbs %*% gammas0 - x
    rss2 <- fnorm(resid) / n

    # Obtain the diference between errors
    imp <- rss - rss2
    rss <- rss2

    # Increment the iteration
    itr <- itr + 1
  }

  # Compute final coefficients
  alphas <- update_alphas(alphas, zbs, gammas, x)
  gammas <- update_gammas(gammas, zbs, alphas, x)
  betas <- update_betas(betas, x, omegas, zbs)
  omegas <- update_omegas(omegas, x, betas, zbs)

  res = list(archetypes=zbs,
             rss=rss,
             alphas=alphas,
             gammas=gammas,
             betas=betas,
             omegas=omegas,
             k=k,
             c=c)
  class(res) <- "biaa"
  
  return(res)
}
