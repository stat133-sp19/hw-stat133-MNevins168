#' @export
plot.bincum <- function(bincumvar) {
  xval <- bincumvar$datafr[,1]
  yval <- bincumvar$datafr[,3]
  plot(xval, yval, type = "o", cex = 1.2, xlab = "successes", ylab = "probability", las = 1)
}


#' @export
plot.bindis <- function(bindisvar) {
  barplot(height = bindisvar$datafr[,2], names.arg = bindisvar$datafr[,1], xlab = "successes", ylab = "probability", las = 1)
}