#' @export
print.combtest <- function(x, ...) {
  msg1 <- paste(" Test:\t", x$test, "\n")
  msg2 <- paste("Data:\t", x$data.name, "\n")
  msg3 <- paste("Piepho Test: Statistics = ", round(x$Piepho_Stat, 5), ", Pvalue = ", round(x$Boik_pvalue, 5),"\n")
  msg4 <- paste("Boik Test: Statistics = ", round(x$Boik_Stat, 5),", Pvalue = ", round(x$Boik_pvalue, 5),"\n")
  msg5 <- paste("Malik Test: Statistics = ", round(x$Malik_Stat, 5),", Pvalue = ", round(x$Malik_pvalue, 5),"\n")
  msg6 <- paste("KKM Test: Statistics = ", round(x$KKM_Stat, 5),", Pvalue = ", round(x$KKM_pvalue, 5),"\n")
  msg7 <- paste("KKSA Test: Statistics = ", round(x$KKSA_Stat, 5),", Pvalue = ", round(x$KKSA_pvalue, 5),"\n")
  msg8 <- paste("Malik Test: Statistics = ", round(x$Franck_Stat, 5),", Pvalue = ", round(x$Franck_pvalue, 5),"\n")
  msg9 <- paste("Bonferroni method: Pvalue =", round(x$Bonferroni, 5),"\n")
  msg10 <- paste("Sidak method: Pvalue =", round(x$Sidak, 5),"\n")
  msg11 <- paste("Jacobi method: Pvalue =", round(x$Jacobi, 5),"\n")
  msg12 <- paste("Gaussian copula: Pvalue =", round(x$GC, 5),"\n")
  cat(msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8, msg9, msg10, msg11, msg12)
}
