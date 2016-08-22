#' @title MR2phy
#' @description A script to convert a metagenomeseq (Biobase) object to a phyloseq object.
#' @param obj A metagenomeseq object
#' @param norm TRUE/FALSE if normalized data should be put into phy object
#' @param log TRUE/FALSE if log transformated data should be put into phy object
#' @return A phy object
#' @examples
#' newphy<-MR2phy(obj)
MR2phy<- function (obj, norm = TRUE, log = FALSE, taxa_are_rows = TRUE) 
{
  tt <- fData(obj)
  TT <- tax_table(as(tt[, -1], "matrix"))
  PT <- sample_data(as(pData(obj), "data.frame"))
  OT <- otu_table(as(MRcounts(obj, norm = norm, log = log), 
                     "matrix"), taxa_are_rows = taxa_are_rows)
  newphy <- phyloseq(OT, PT, TT)
}