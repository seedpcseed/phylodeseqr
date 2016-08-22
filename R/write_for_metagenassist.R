#' Output a phyloseq dataset for analysis in Metagenassist
#' 
#' @param phy A phyloseq S4 object
#' @param base.name The base name for writing files
#' 
write_for_metagenassist<-function(phy,base.name){
  m<-as.matrix(otu_table(phy))
#   dd<-dim(m)
#   colnames(m)<-c(1:dd[2])
  
  d<-data.frame('#SampleID'=
                paste("root; cellular organisms",
                      tax_table(phy)[,"Domain"],
                      unlist(lapply(tax_table(phy)[,"Phylum"], function (x) strsplit(as.character(x),"__")[[1]][2])),
                      unlist(lapply(tax_table(phy)[,"Class"], function (x) strsplit(as.character(x),"__")[[1]][2])),
                      unlist(lapply(tax_table(phy)[,"Order"], function (x) strsplit(as.character(x),"__")[[1]][2])),
                      unlist(lapply(tax_table(phy)[,"Family"], function (x) strsplit(as.character(x),"__")[[1]][2])),
                      unlist(lapply(tax_table(phy)[,"Genus"], function (x) strsplit(as.character(x),"__")[[1]][2])),
                      rownames(otu_table(phy)),
                      sep=";"), m, check.names=FALSE)
  colnames(d)[1]<-"#SampleID"

  write.csv(d,paste("OTU_TABLE_",base.name,".csv",sep=""), row.names=FALSE)
  
  d<-data.frame(sample_data(phy))
#   dd<-dim(d)
#   rownames(d)<-d[,1]<-c(1:dd[1])
  colnames(d)[1]<-"SampleID"
  write.csv(d,paste("SAMPLE_DATA_",base.name,".csv",sep=""), row.names=FALSE)
}
  