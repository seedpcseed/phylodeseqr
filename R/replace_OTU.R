#' @name replace_OTU
#' @title Replaces the otu_table in a phyloseq object with a new matrix
#' 
#' @details Providing a new matrix and a phyloseq object phy to the function, the old OTU table will be replaced with a new matrix of OTU counts. The remaining parts of the S4 objedct will be adjusted accordingly.
#' 
#' @param physeq A phyloseq object
#' @param mat A matrix of new OTU counts with samples in columns and OTU in rows (and OTU names in rownames).
#' @return A revised phyloseq S4 object with the new OTU counts.
#' @rdname replace_OTU
#' @export
#' @examples
#' 
#' #' replace_OTU(phy=phylo1, mat=newOTUs) 

replace_OTU<-function (physeq, mat) 
{
  if (is.null(otu_table(physeq))) {
    cat("Nothing to replace No otu_table in physeq.\n")
    return(physeq)
  }
  else {
    otu_table(physeq) <- otu_table(as(mat,"matrix"),taxa_are_rows=TRUE)
    return(physeq)
    }
  
}