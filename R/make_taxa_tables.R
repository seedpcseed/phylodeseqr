#' A function to organize a phyloseq S4 object into 
#' tables for use in classification modelling
#' @param phy A phyloseq S4 object
#' @return A dataTable list with per taxa category dataframes of counts
#' 
make_taxa_tables<-function(phy){
  dataTable<<-list()
  base.mat<-as.matrix(t(otu_table(phy)))
  base.tax<-as.data.frame(tax_table(phy))
  taxa<-c("Phylum","Class","Order","Family","Genus")
  for(i in taxa){
    if(i!="Phylum"){
      c<-unlist(lapply(base.tax[,i], function(x) strsplit(as.character(x),"__")[[1]][2]))
      c.index<-grep("g",c)
      c.replace<-base.tax[unlist(c.index),taxa[grep(i,taxa)-1]]
      c<-replace(c,c.index,as.character(c.replace))
        colnames(base.mat)<-c
      #colnames(base.mat)<-strsplit(base.tax[,i],"__")[[1]][2]
    }
    else{
      colnames(base.mat)<-base.tax[,i]
    }
    dataTable[[i]]<-base.mat
  }
  dataTable$all.taxa<-cbind(dataTable$Phylum,
                            dataTable$Class,
                            dataTable$Order,
                            dataTable$Family,
                            dataTable$Genus)
  return(dataTable)
}