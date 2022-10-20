#' @export
print.combtest <- function(x, ...) {
  msg1 <- paste(" Test:\t", x$test, "\n")
  msg2 <- paste("Data:\t", x$data.name, "\n")
  msg3 <- paste("Piepho Test: Statistic = ", round(x$Piepho_Stat, 5), ", Pvalue = ", round(x$Piepho_pvalue, 5), "\n")
  msg4 <- paste("Boik Test: Statistic = ", round(x$Boik_Stat, 5), ", Pvalue = ", round(x$Boik_pvalue, 5), "\n")
  msg5 <- paste("Malik Test: Statistic = ", round(x$Malik_Stat, 5), ", Pvalue = ", round(x$Malik_pvalue, 5), "\n")
  msg6 <- paste("KKM Test: Statistic = ", round(x$KKM_Stat, 5), ", Pvalue = ", round(x$KKM_pvalue, 5), "\n")
  msg7 <- paste("KKSA Test: Statistic = ", round(x$KKSA_Stat, 5), ", Pvalue = ", round(x$KKSA_pvalue, 5), "\n")
  msg8 <- paste("Franck Test: Statistic = ", round(x$Franck_Stat, 5), ", Pvalue = ", round(x$Franck_pvalue, 5), "\n")
  msg9 <- paste("Bonferroni method: Pvalue =", round(x$Bonferroni, 5), "\n")
  msg10 <- paste("Sidak method: Pvalue =", round(x$Sidak, 5), "\n")
  msg11 <- paste("Jacobi method: Pvalue =", round(x$Jacobi, 5), "\n")
  msg12 <- paste("Gaussian copula: Pvalue =", round(x$GC, 5), "\n")
  msg15 <- paste("------------------------------------------------", "\n")
  msg13 <- paste("The result of the combined test at the", paste0(100 * (x$Level), "%"), "level:", "\n")
  msg14 <- paste(x$Result, "\n")
  cat(justify(msg1), justify(msg2), justify(msg3), justify(msg4), justify(msg5), justify(msg6), justify(msg7), justify(msg8), justify(msg9), justify(msg10), justify(msg11), justify(msg12), justify(msg15), justify(msg13), justify(msg14))
}
