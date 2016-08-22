#PLS-DA analysis

require(mixOmics)

#' This is a function to make the PLDA matrix using 
#' the mixOmics package.
#' @param phy A phyloseq S4 object
#' @param category A grouping category to contrast
#' @return A list and dataframe with the PLSDA information
#' @examples
#' plsda.phy<-make_plsda(phy0,"Group")
make_plsda<-function(phy,category, sparse=FALSE){
  otus<-t(otu_table(phy))
  samples<-sample_data(phy)
  cats<-samples[,category]
  X<-otus
  Y<-cats[[1]]
  if(!sparse){plsda.phy<-plsda(X,Y,ncomp=3)}
  else{plsda.phy<-splsda(X,Y,ncomp=3, keepX=c(25,25,25))}
  
  col.rat <- as.numeric(as.factor(Y))
  return(list(plsda=plsda.phy,col.rat=col.rat))
}




            