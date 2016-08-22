#' @name fishersMRobject
#' @title Performs a Fisher's exact test for 2 outcomes from a Metagenomeseq object
#' @details Sometimes differences between positive and negative measures in samples between groups require statistical evaluation. This can be done using the Chi Square or Fisher's exact test.  
#' @param obj A metagenomeseq object
#' @param otulist A list of the otu that should be used in the test.
#' @param classIndex A list of samples in each of the test groups
#' @param norm TRUE or FALSE to use normalized data.
#' @return Returns a p value for a Fisher exact test.
#' @rdname fishersMRobject
#' @export
#' @examples 
#' otulist=which(fData(obj)[rank]==testtax)
#' classIndex=list(unexposed=which(pData(objall.controls)$hiv_exp=="HIVUnexposed"))
#' classIndex$exposed=which(pData(objall.controls)$hiv_exp=="HIVExposed")
#' fishersMRobject<-function (obj=objall.controls, otulist=otulist,classIndex=classIndex, norm = TRUE)
fishersMRobject<-function (obj, otulist,classIndex, norm = TRUE, thresh=0) 
{
  mat = MRcounts(obj, norm = norm, log=FALSE)
  l = lapply(classIndex, function(j) {mat[otulist, j]})
  l.1.p<-length(which(sapply(l[1],function(j){j>thresh})))
  l.1.n<-length(which(!sapply(l[1],function(j){j>thresh})))
  l.2.p<-length(which(sapply(l[2],function(j){j>thresh})))
  l.2.n<-length(which(!sapply(l[2],function(j){j>thresh})))
  ft<-matrix(c(l.1.p,l.2.p,l.1.n,l.2.n),nrow=2,ncol=2, dimnames=list(c(names(l[1]),names(l[2])),c("Positive","Negative")))
  fisher.test(ft)
}