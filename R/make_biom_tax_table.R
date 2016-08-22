#' make_biom_tax_table
#' @description To take a phyloseq taxa table and convert to a biom compatible table
#' @param phy a phyloseq object
#' @examples 
#' new_biom_table<-make_biom_table(phy1)
make_biom_tax_table<-function(phy){
  tt<-as(tax_table(phy),"matrix")
  colnames(tt)<-c("taxonomy1","taxonomy2","taxonomy3","taxonomy4","taxonomy5","taxonomy6")
  lapply(rownames(tt)[1:nrow(tt)], function(x) {
    ll=list(tt[x,])
  })
  
  
  #tt<-transform(tt,OTU=rownames(tt))
  id<-rownames(tt)[1]
  d<-tt[1,]
  l<-list(d)
  names(l[1])=id
  tt.s<-split(tt[1:6],tt$OTU)
}