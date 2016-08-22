#' A function to melt data from an S4 phyloseq table with metadata
#' 
#' @param phy.in An S4 phyloseq object
#' @return A dataframe with the merged data
#' @export
#' @examples
#' otu2melt(phy0)
otu2melt<-function(phy.in){
  phy.melt<-psmelt(phy.in)
  otu.temp<-otu_table(phy.in)
  otu.temp<-data.frame(OTU=rownames(otu.temp),otu.temp)
  phy.merge<-merge(phy.melt,otu.temp,by="OTU")
  head(phy.merge)
  return(phy.merge)
}
