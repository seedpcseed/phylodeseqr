#' @name objmelt
#' @title Melts an MRexperiments object into a single dataframe.
#' 
#' @details Using the an MRexperiment, the phenotype and count data will be combined into a single long dataframe object.
#' 
#' @param obj A MRexperiment object.
#' @param norm TRUE-FALSE to use normalized data
#' @param log TRUE-FALSE to use log transformed data
#' @param sl An integer to use to expand the counts
#' @return A long data.frame
#' @rdname objmelt
#' @export
#' @examples
#' 
#' data(mouseData)
#' om <- objmelt(mouseData, norm=TRUE, log=FALSE, sl=1000)
#' 
objmelt<- function (obj.in, norm=TRUE, log=FALSE, sl=1000) 
{
  if (!inherits(obj.in, "MRexperiment")) {
    rankNames = NULL
    sampleVars = NULL
  } else {
    rankNames = colnames(fData(obj.in))
    sampleVars = colnames(pData(obj.in))
  }
  reservedVarnames = c("Sample", "Abundance", "OTU")
  type1aconflict = intersect(reservedVarnames, sampleVars)
  if (length(type1aconflict) > 0) {
    wh1a = which(sampleVars %in% type1aconflict)
    new1a = paste0("sample_", sampleVars[wh1a])
    warning("The sample variables: \n", paste(sampleVars[wh1a], 
                                              collapse = ", "), "\n have been renamed to: \n", 
            paste0(new1a, collapse = ", "), "\n", "to avoid conflicts with special phyloseq plot attribute names.")
    colnames(pData(obj.in))[wh1a] <- new1a
  }
  type1bconflict = intersect(reservedVarnames, rankNames)
  if (length(type1bconflict) > 0) {
    wh1b = which(rankNames %in% type1bconflict)
    new1b = paste0("taxa_", rankNames[wh1b])
    warning("The rank names: \n", paste(rankNames[wh1b], 
                                        collapse = ", "), "\n have been renamed to: \n", 
            paste0(new1b, collapse = ", "), "\n", "to avoid conflicts with special phyloseq plot attribute names.")
    colnames(fData(obj.in))[wh1b] <- new1b
  }
  type2conflict = intersect(sampleVars, rankNames)
  if (length(type2conflict) > 0) {
    wh2 = which(sampleVars %in% type2conflict)
    new2 = paste0("sample_", sampleVars[wh2])
    warning("The sample variables: \n", paste0(sampleVars[wh2], 
                                               collapse = ", "), "\n have been renamed to: \n", 
            paste0(new2, collapse = ", "), "\n", "to avoid conflicts with taxonomic rank names.")
    colnames(pData(obj.in))[wh2] <- new2
  }
  otutab = MRcounts(obj.in, norm=norm, log=log, sl=sl)
  mdf = reshape2::melt(as(otutab, "matrix"))
  colnames(mdf)[1] <- "OTU"
  colnames(mdf)[2] <- "Sample"
  colnames(mdf)[3] <- "Abundance"
  mdf$OTU <- as.character(mdf$OTU)
  mdf$Sample <- as.character(mdf$Sample)
  if (!is.null(sampleVars)) {
    sdf = data.frame(pData(obj.in), stringsAsFactors = FALSE)
    sdf$Sample <-Biobase::sampleNames(obj.in)
    mdf <- merge(mdf, sdf, by.x = "Sample")
  }
  if (!is.null(rankNames)) {
    TT = fData(obj.in)
    colnames(TT)[1]<- "OTU"
    keepTTcols <- colSums(is.na(TT)) < length(featureNames(obj.in))
    if (length(which(keepTTcols)) > 0 & ncol(TT) > 0) {
      tdf <- TT[, keepTTcols]
      mdf <- merge(mdf, tdf, by= "OTU")
    }
  }
  mdf = mdf[order(mdf$Abundance, decreasing = TRUE), ]
  return(mdf)
}