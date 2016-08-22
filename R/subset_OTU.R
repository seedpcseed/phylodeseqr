subset_OTU<-function (physeq, keep) 
{
  if (is.null(otu_table(physeq))) {
    cat("Nothing subset. No otu_table in physeq.\n")
    return(physeq)
  }
  else {
    oldMA <- as(otu_table(physeq), "matrix")
    oldDF <- data.frame(oldMA)
    newDF <- oldDF[keep,]
    if (inherits(physeq,"otu_table")) {
      return(otu_table(newDF))
    }
    else {
      otu_table(physeq) <- otu_table(as(newDF,"matrix"),taxa_are_rows=TRUE)
      return(physeq)
    }
  }
}