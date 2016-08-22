#' @name phy2MR
#' @title Make a metagenomeSeq object from a phyloseq object
#' 
#' @details Takes a phyloseq object and a p value (provided or as cumNormStat) and outputs a metagenomeSeq object.
#' 
#' @param physeq A phyloseq object.
#' @param p A quantile value.
#' @return A metagenomeSeq object.
#' @rdname phy2MR
#' @export
#' @examples
#' 
#' obj = phy2MR(phy1, p=0.5)


phy2MR<- function(physeq, p=0.5) {
  require("metagenomeSeq")
  require("phyloseq")
  # Enforce orientation
  if (!taxa_are_rows(physeq)) {
    physeq <- t(physeq)
  }
  OTU = as(otu_table(physeq), "matrix")
  # Convert sample_data to AnnotatedDataFrame
  ADF = AnnotatedDataFrame(data.frame(sample_data(physeq)))
  # define dummy 'feature' data for OTUs, using their name Helps with
  # extraction and relating to taxonomy later on.
  TDF = AnnotatedDataFrame(data.frame(OTUname = taxa_names(physeq), tax_table(physeq),row.names = taxa_names(physeq)))
  # Create the metagenomeSeq object
  MGS = newMRexperiment(counts = OTU, phenoData = ADF, featureData = TDF)
  # Trigger metagenomeSeq to calculate its Cumulative Sum scaling factor.
  MGS = cumNorm(MGS, p=p)
  return(MGS)
}