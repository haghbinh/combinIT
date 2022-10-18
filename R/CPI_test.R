#' Combined P-value Interaction Test
#'
#' This function reports the p-values of the tests for non-additivity developed by Boik (1993), Piepho (1994),
#' Kharrati-Kopaei and Sadooghi-Alvandi (2007), Franck et al. (2013), Malik et al. (2016)
#' and Kharrati-Kopaei and Miller (2016). In addition, it combines the p-values of these six methods into a single p-value as a test statistic for testing interaction.
#' There are four combination methods:
#' Bonferroni, Sidak, Jacobi expansion, and Gaussian Copula. The results of these four combinations are also reported. If there is a significant interaction, the type of interaction is also provided.
#'
#' @param x numeric matrix, \eqn{b \times a} data matrix where the number of rows and columns are corresponding to the block and treatment levels, respectively.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param nc0 a numeric value, the number of Monte Carlo samples for computing the unbiasing constant \eqn{c_0}. The default value is 10000.
#' @param Elapsed.time logical: if \code{TRUE} the progress will be printed in the console.
#'
#' @details If rows number of data matrix, \eqn{b}, is less than it's columns number, \eqn{a},
#'  the data matrix is transposed. In addition, this test procedure requires that the data matrix has at least two
#'  rows or columns. Note that the KKSA.test is not applicable when both \eqn{a} and \eqn{b} are less than 4. This function needs "mvtnorm" package.
#'
#' @return An object of the class \code{combtest}, which is a list inducing following components::
#' \item{nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{Piepho_pvalue}{The p-value of Piepho's (1994) test.}
#' \item{Piepho_Stat}{The value of Piepho's (1994) test statistic.}
#' \item{Boik_pvalue}{The p-value of Boik's (1993) test.}
#' \item{Boik_Stat}{The value of Boik's (1993) test statistic.}
#' \item{Malik_pvalue}{The p-value of Malik's (2016) et al. test.}
#' \item{alik_Stat}{The value of Malik's (2016) et al. test statistic.}
#' \item{KKM_pvalue}{The p-value of Kharrati-Kopaei and Miller's (2016) test.}
#' \item{KKM_Stat}{The value of Kharrati-Kopaei and Miller's (2016) test statistic.}
#' \item{KKSA_pvalue}{The p-value of Kharrati-Kopaei and Sadooghi-Alvandi's (2007) test.}
#' \item{KKSA_Stat}{The value of Kharrati-Kopaei and Sadooghi-Alvandi's (2007) test statistic.}
#' \item{Franck_pvalue}{The p-value of Franck's (2013) et al. test.}
#' \item{Franck_Stat}{The value of Franck's (2013) et al. test statistic.}
#' \item{Bonferroni}{The combined p-value by using the Bonferroni method.}
#' \item{Sidak}{The combined p-value by using the Sidak method.}
#' \item{Jacobi}{The combined p-value by using the Jacobi method.}
#' \item{GC}{The combined p-value by using the Gaussian copula.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' 
#'
#' @references Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.
#'  
#' @examples
#' data(RDWW)
#' CPI_test(RDWW, nsim = 1000, Elapsed_time = FALSE)
#' 
#' @importFrom stats pchisq pf qnorm var
#' @export
CPI_test <- function(x, nsim = 10000, nc0 = 10000, Elapsed_time = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    y <- c(t(x))
    tr <- ncol(x)
    bl <- nrow(x)
    if (bl < tr) {
      warning("The input matrix data was transposed")
      x <- t(x)
      te <- bl
      bl <- tr
      tr <- te
    }
    if (bl <= 3) {
      warning("KKSA_test needs at least 4 levels for a factor. For combining pvalues, the pvalue of KKSA method is not considered.")
    }
    n <- tr * bl
    block <- gl(bl, tr)
    treatment <- gl(tr, 1, bl * tr)
    p <- min(tr - 1, bl - 1)
    q <- max(tr - 1, bl - 1)
    cck <- 2^(bl - 1) - 1 - bl
    cch <- 2^(bl - 1) - 1
    kp <- kpr(bl, tr)
    c0 <- mean(replicate(nc0, {
      median(abs(kp %*% rnorm(n)))
    }))
    sta <- bmp_f(x)
    Bstat <- sta$Boik
    Mstat <- sta$Tc
    pistat <- sta$piepho
    pstat <- picf(y, kp, c0)
    if (bl == 3) {
      Hstat <- hh_f(x)
    } else {
      Ksimu <- rep(0, 0)
      kh <- kh_f(x)
      Kstat <- kh$fmin
      Hstat <- kh$fmax
    }
    Bsimu <- Msimu <- psimu <- pisimu <- Hsimu <- rep(0, 0)
    if (Elapsed_time) {
      pb <- completed(nsim)
      for (i in 1:nsim) {
        y <- rnorm(n)
        x <- matrix(y, nrow = bl, byrow = TRUE)
        sta <- bmp_f(x)
        Bsimu[i] <- sta$Boik
        Msimu[i] <- sta$Tc
        pisimu[i] <- sta$piepho
        psimu[i] <- picf(y, kp, c0)
        if (bl == 3) {
          Hsimu[i] <- hh_f(x)
        } else {
          kh <- kh_f(x)
          Ksimu[i] <- kh$fmin
          Hsimu[i] <- kh$fmax
        }
        if (i == pb$pr[pb$j]) pb <- nextc(pb, i)
      }
    } else {
      for (i in 1:nsim) {
        y <- rnorm(n)
        x <- matrix(y, nrow = bl, byrow = TRUE)
        sta <- bmp_f(x)
        Bsimu[i] <- sta$Boik
        Msimu[i] <- sta$Tc
        pisimu[i] <- sta$piepho
        psimu[i] <- picf(y, kp, c0)
        if (bl == 3) {
          Hsimu[i] <- hh_f(x)
        } else {
          kh <- kh_f(x)
          Ksimu[i] <- kh$fmin
          Hsimu[i] <- kh$fmax
        }
      }
    }
    Tb <- (1 / Bstat - 1)
    if (p == 1) {
      Boik_pvalue <- 1
    }
    if (p == 2) {
      Boik_pvalue <- 1 - pbeta(Tb, 1, (q - 1) / 2)
    }
    if (p > 2) {
      Boik_pvalue <- mean(Bstat >= Bsimu)
    }
    piepho_pvalue <- mean(pistat < pisimu)
    PIC_pvalue <- mean(pstat < psimu)
    Malik_pvalue <- mean(Mstat < Msimu)
    hiddenf_pvalue <- mean(Hstat < Hsimu)
    if (bl <= 3) {
      KKSA_pvalue <- NA
    } else {
      KKSA_pvalue <- mean(Kstat > Ksimu)
    }
    pvalues <- c(Boik_pvalue, piepho_pvalue, hiddenf_pvalue, Malik_pvalue, PIC_pvalue, KKSA_pvalue)
    if (bl <= 3) {
      pvalues <- pvalues[!is.na(pvalues)]
    } else {
      pvalues <- pvalues
    }
    cp <- comb(pvalues)
    Bonferroni <- cp$Bon
    GC <- cp$GC
    Sidak <- cp$Sidak
    jacobi <- cp$jacobi
    if (cp$Bon >= 0.05 & cp$GC >= 0.05 & cp$Sidak >= 0.05 & cp$jacobi >= 0.05) message("No significant interaction type was detected at the 5% level")
    if ((cp$Bon < 0.05 | cp$Sidak < 0.05 | cp$jacobi < 0.05) & bl >= 4) {
      message("There are significant interaction types at the 5% level")
      if (min(pvalues) == Boik_pvalue) message("The multiplicative form of interaction migth exist")
      if (min(pvalues) == piepho_pvalue) message("The detected significant interaction might due to the Grubbs type estimators of variances are heterogeneous across the levels of one factor")
      if (min(pvalues) == hiddenf_pvalue) message("A hidden structure of intercation might exist")
      if (min(pvalues) == Malik_pvalue) message("Some cells produce large negative or positive residuals due to the significant interaction")
      if (min(pvalues) == PIC_pvalue) message("Significant interactions are caused by some cells")
      if (min(pvalues) == KKSA_pvalue) message("The magnitude of interaction effects is heteroscedastic across the sub-tables of observations")
    }
    if ((cp$Bon < 0.05 | cp$Sidak < 0.05 | cp$jacobi < 0.05) & bl < 4) {
      message("There are significant interaction types at the 5% level")
      if (min(pvalues) == Boik_pvalue) message("The multiplicative form of interaction migth exist")
      if (min(pvalues) == piepho_pvalue) message("The detected significant interaction might due to the Grubbs type estimators of variances are heterogeneous across the levels of one factor")
      if (min(pvalues) == hiddenf_pvalue) message("A hidden structure of intercation might exist")
      if (min(pvalues) == Malik_pvalue) message("Some cells produce large negative or positive residuals due to the significant interaction")
      if (min(pvalues) == PIC_pvalue) message("Significant interactions are caused by some cells")
    }
    if (bl < 4) {
      KKSA_pvalue <- NA
      Kstat <- NA
    } 
    out <- list(
      nsim = nsim,
      Piepho_pvalue = piepho_pvalue,
      Piepho_Stat = pistat,
      Boik_pvalue = Boik_pvalue,
      Boik_Stat = Bstat,
      Malik_pvalue = Malik_pvalue,
      Malik_Stat = Mstat,
      KKM_pvalue = PIC_pvalue,
      KKM_Stat = pstat,
      KKSA_pvalue = KKSA_pvalue,
      KKSA_Stat = Kstat,
      Franck_pvalue = hiddenf_pvalue,
      Franck_Stat = Hstat,
      Bonferroni = Bonferroni,
      Sidak = Sidak,
      Jacobi = jacobi,
      GC = GC,
      data_name = DNAME,
      test = "Combined p-value interaction Test"
    )
    structure(out, class = "combtest")
  }
}

