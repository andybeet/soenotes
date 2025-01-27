#' Shannon Index example
#'
#' Demonstrate some of the issues in interpreting changes in the index
#'
#' Sampling ps sequentially from uniform subject to sum(p) = 1
#' Skews to top heavy populations

shannon_skewed <- function(nSim = 1000, nspV=10:20) {

insp <- 0
shannonM <- array(data = NA,dim = c(length(nspV),nSim))
maxShannon <-  array(data=NA, dim = c(length(nspV),2))
for(nsp in nspV) {
  insp <- insp + 1
  shannonV <- vector(mode = "numeric", length = nSim)
  maxShannon[insp,1] <- nsp
  maxShannon[insp,2] <- -sum(rep(1/nsp,nsp)*log(rep(1/nsp,nsp)))

  for (i in 1:nSim) {
    pV <- vector(mode="numeric", length = nsp)
    plim <- 1
    for (ip in 1:(nsp-1)) {
      p <- runif(1,0,plim)
      pV[ip] <- p
      plim <- plim - p
    }
    pV[nsp] <- 1 - sum(pV)
    shannon <- -sum(pV*log(pV))
    shannonV[i] <- shannon

  }
  shannonM[insp,] <- shannonV
}

a <- cbind(as.data.frame(nspV),as.data.frame(shannonM))
newD <- a |>
  tidyr::pivot_longer(cols=-nspV, names_to = "names",values_to = "value") |>
  dplyr::select(-names)

# maxShannon <- as.data.frame(maxShannon) |>
#   dplyr::as_tibble() |>
#   dplyr::rename(nspV = V1,
#                 value = V2)


#newD <- rbind(newD,maxShannon)

pplot <- newD |>
  ggplot2::ggplot(ggplot2::aes(y = value)) +
  # ggplot2::geom_point() +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::facet_grid(cols=dplyr::vars(nspV),scales = "free_x") +
  ggplot2::theme(axis.text.x=ggplot2::element_blank())+
  ggplot2::xlab("Number of species") +
  ggplot2::ylab("Shannon Index (-sum(p*log(p)))") +
  ggplot2::ggtitle("Skewed sampling")

  return(pplot)

}
