#' Malik (2016) et al. Test for Interaction
#'
#' The Malik's (2016) et al. test statistics is calculated and the corresponding exact p-value is calculated by a Monte Carlo simulation.
#'
#' @param x numeric matrix, \eqn{b \times a} data matrix where the number of rows and columns are corresponding to the block and treatment levels, respectively.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param Elapsed.time logical: if \code{TRUE} the progress will be printed in the console.
#' 
#' @return An object of the class \code{ITtest}, which is a list inducing following components:
#' \item{pvalue.exact}{The calculated exact Monte Carlo p-value.}
#' \item{pvalue.appro}{is not available for \code{Malik.test}.}
#' \item{statistic}{The value of the test statistic.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{data.name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' 
#' @details
#'  Malik (2016) et al. proposed to partition
#'  the residuals into three clusters using a suitable clustering method like “k-means clustering”.
#'  The hypothesis of no interaction can be interpreted as the effect of the three
#'  clusters are equal. Therefore, the result of the test may depend on the method of clustering. In this package, clustering is done by 'kmeans' function in 'RcppArmadillo'. The 'speed_mode' parameter on the kmeans clustering was set as 'static_subset'.
#'  Note that the Malik's et al. test performs well when there are some outliers in the residuals; i.e. some cells produce large negative or positive residuals due to the significant interaction.
#'  Further, the distribution of the Malik's et al. test statistic is not known under additivity and the corresponding p-value is calculated by a Monte Carlo simulation.

#' @references Malik, W.A., Mohring, J., Piepho, H.P. (2016). A
#' clustering-based test for non-additivity in an unreplicated two-way layout.
#' Communications in Statistics-Simulation and Computation 45(2):660-670.
#'
#' Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#' Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#' 86(3): 469-487.
#' 
#' @examples
#' data(IDCP)
#' Malik.test(IDCP, nsim = 1000, Elapsed.time = FALSE)
#' 
#' @export
Malik.test <- function(x, nsim = 10000, Elapsed.time = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    tr <- ncol(x)
    bl <- nrow(x)
    n <- tr * bl
    block <- gl(bl, tr)
    treatment <- gl(tr, 1, bl * tr)
    y <- c(t(x))
    statistic <- M_f(x)
    simu <- rep(0, 0)
    if (Elapsed.time) {
      pb <- completed(nsim)
      for (i in 1:nsim) {
        x0 <- matrix(rnorm(n), nrow = bl)
        y0 <- c(t(x0))
        simu[i] <- M_f(x = x0)
        if (i == pb$pr[pb$j]) pb <- nextc(pb, i)
      }
    } else {
      for (i in 1:nsim) {
        x0 <- matrix(rnorm(n), nrow = bl)
        y0 <- c(t(x0))
        simu[i] <- M_f(x = x0)
      }
    }
    malik <- mean(statistic < simu)
    structure(
      list(
        pvalue.exact = malik,
        pvalue.appro = "NULL",
        nsim = nsim,
        statistic = statistic,
        data.name = DNAME,
        test = "Malik Test"
      ),
      class = "ITtest"
    )
  }
}
