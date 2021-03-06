#' Kharrati-Kopaei and Miller's (2016) Test for Interaction
#'
#' This function calculates the test statistic for testing \eqn{H_0:} no interaction and corresponding Monte Carlo p-value
#' proposed by Kharrati-Kopaei and Miller(2016).
#'
#' @param x a numerix matrix, \eqn{b \times a} data matrix where the number of rows and columns are corresponding to the block and treatment levels, respectively.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param nc0 a numeric value, the number of Monte Carlo samples for computing the unbiasing constant \eqn{c_0}. The default value is 10000.
#'
#' @return An object of the class \code{ITtest}, which is a list inducing following components::
#' \item{pvalue.exact}{The calculated exact Monte Carlo p-value.}
#' \item{pvalue.appro}{is not available for \code{KKM.test}.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{statistic}{The value of the test statistic.}
#' \item{data.name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#'
#' @details
#' Kharrati-Kopaei and Miller(2016) proposed a test statistic for testing interaction
#' based on inspecting all pairwise interaction contrasts (PIC).
#' This test depends on an unbiasing constant \eqn{c_0} that is calculated by a Monte Carlo simulation.
#' In addition, the null distribution of the test statistic is calculated by a Monte Carlo simulation.
#' Note that this test procedure is powerful when significant interactions are caused by some data cells.
#'
#' @references Kharrati-Kopaei, M., Miller, A. (2016). A method for testing interaction in
#'  unreplicated two-way tables: using all pairwise interaction contrasts. Statistical
#'  Computation and Simulation 86(6):1203-1215.
#'
#'  Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.

#' @examples
#' data(RDWW)
#' KKM.test(RDWW, nsim = 1000, nc0 = 1000)
#' 
#' @export
KKM.test <- function(x, nsim = 1000, nc0 = 10000) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    y <- c(t(x))
    tr <- ncol(x)
    bl <- nrow(x)
    n <- tr * bl
    kp <- kpr(bl, tr)
    c0 <- C0(kp, n, nc0)
    statistics <- picf(y, kp, c0)
    simu <- PICfsim(nsim, kp, c0, n)
    PIC <- mean(statistics < simu)
  }
  structure(
    list(
      pvalue.exact = PIC,
      pvalue.appro = "NULL",
      nsim = nsim,
      statistic = statistics,
      data.name = DNAME,
      test = "KKM Test"
    ),
    class = "ITtest"
  )
}