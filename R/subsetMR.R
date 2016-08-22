#' @name subset_MR
#' @title Subsets a metagenome object by a sample variable like subset does
#' @param obj A metagenomeseq  objects (from Biobase)
#' @param feature Conditions for subsetting in the form .(xxx)
#' @examples 
#' newObj<-subset_MR(obj,sampleType=="red")
subset_MR<-function(obj,feature){
  mat<-MRcounts(obj)
  pd<-pData(obj)
  #eval.form<-paste("pd$",feature,sep="")
  pd<-subset(pd, eval(feature[[1]]))
  #pd<-pd[eval.form,]
  keep<-rownames(pd)
  mat<-mat[,keep]
  pd = as(pd,"AnnotatedDataFrame")
  fd = as(fData(obj),"AnnotatedDataFrame")
  newObj = newMRexperiment(mat,featureData=fd,phenoData=pd)
  return(newObj)
}