#' A script to convert a phyloseq S4 object to a 
#' metagenomeSeq S4 object 
#' (from http://joey711.github.io/waste-not-supplemental/
#' simulation-differential-abundance/
#' simulation-differential-abundance-server.html)
#' 
#' @param physeq An S4 phyloseq object
#' @export
#' @return A metagenomeSeq object
#' @examples 
#' obj<-make_metagenomeSeq(obj)
make_metagenomeSeq = function(physeq) {
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
  MGS = cumNorm(MGS)
  return(MGS)
}