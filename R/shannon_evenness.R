#' Shannon Index example
#'
#' Demonstrate some of the issues in interpreting changes in the index
#' Evenness in p's
#' @param nSim numeric. Number of simulations
#' @param nspV numeric vector. Number of entities to simulate
#'
#'
#' @export

shannon_evenness <- function(nSim = 1000, nspV=10:20) {

  insp <- 0
  shannonM <- array(data = NA,dim = c(length(nspV),nSim))
  for(nsp in nspV) {
    insp <- insp + 1
    shannonV <- vector(mode = "numeric", length = nSim)

    # simulate ps from uniform then divide by sum
    # on average they will hover around evenness
    for (i in 1:nSim) {
      p <- runif(nsp,0,1)
      pV <- p/sum(p)
      shannon <- -sum(pV*log(pV))
      shannonV[i] <- shannon
    }
    shannonM[insp,] <- shannonV
  }

  a <- cbind(as.data.frame(nspV),as.data.frame(shannonM))
  newD <- a |>
    tidyr::pivot_longer(cols=-nspV, names_to = "names",values_to = "value") |>
    dplyr::select(-names)

  pplot <- newD |>
    ggplot2::ggplot(ggplot2::aes(y = value)) +
    #ggplot2::geom_point() +
    ggplot2::geom_histogram(bins = 50) +
    ggplot2::facet_grid(cols=dplyr::vars(nspV),scales = "free_x") +
    ggplot2::theme(axis.text.x=ggplot2::element_blank())+
    ggplot2::xlab("Frequency") +
    ggplot2::ylab("Shannon Index (-sum(p*log(p)))") +
    ggplot2::ggtitle("Approximating Evenness")

  return(pplot)
}
