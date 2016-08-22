#' @name plotMultiOTU
#' @title Plots multiple OTU for different subject groups.
#' @details OTU counts from within a taxon may be plot for different subject groups.
#' @param obj A metagenomeseq object
#' @param otulist A list of the otu that should be used in the test.
#' @param classIndex A list of samples in each of the test groups
#' @param log TRUE or FALSE to use log2 transformed data.
#' @param norm TRUE or FALSE to use normalized data.
#' @param jitter.factor The distance factor to jitter plotted points.
#' @param pch Character size for labels
#' @param labs TRUE or FALSE to plot labels
#' @param xlab A character string for the X axis title
#' @param ylab A character string for the y axis title.
#' @param jitter TRUE or FALSE to use jitter.factor in plotting points.
#' @param ret TRUE or FALSE to return the plotting data.
#' @return Plots OTUs in groups. If ret=TRUE, plotting data are returned.
#' @rdname plotMultiOTU.R
#' @export
#' @examples 
#' testax=="__Streptococcus"
#' otulist=which(fData(obj)[rank]==testtax)
#' classIndex=list(unexposed=which(pData(objall.controls)$hiv_exp=="HIVUnexposed"))
#' classIndex$exposed=which(pData(objall.controls)$hiv_exp=="HIVExposed")
#' plotMultiOTU(objall.controls, otulist=otulist, classIndex=classIndex)
plotMultiOTU<-function (obj, otulist, classIndex, log = TRUE, norm = TRUE, jitter.factor = 1, 
          pch = 21, labs = TRUE, xlab = NULL, ylab = NULL, jitter = TRUE, 
          ret = FALSE, ...) 
{
  mat = MRcounts(obj, norm = norm, log = log)
  l = lapply(classIndex, function(j) {mat[otulist, j]})
  z = posterior.probs(obj)
  y = unlist(l)
  x = rep(seq(along = l), sapply(l, length))
  if (!is.null(z)) {
    z = 1 - z
    lz = lapply(classIndex, function(j) {
      (z[otu, j])
    })
    z = unlist(lz)
    blackCol = t(col2rgb("black"))
    col = rgb(blackCol, alpha = z)
  }
  else {
    blackCol = t(col2rgb("black"))
    col = rgb(blackCol)
  }
  if (jitter) 
    x = jitter(x, jitter.factor)
  if (is.null(ylab)) {
    ylab = "Normalized log2(counts)"
  }
  if (is.null(xlab)) {
    xlab = "Groups of comparison"
  }
  plot(x, y, col = col, pch = pch, bg = col, xlab = xlab, ylab = ylab, 
       xaxt = "n", ...)
  if (labs == TRUE) {
    gp = names(classIndex)
    axis(1, at = seq(1:length(gp)), gp)
  }
  if (ret) 
    list(x = x, y = y)
}