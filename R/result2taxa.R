#' Add taxa from a phyloseq object to the results analysis of a DESeq object
#' 
#' @param phy A phyloseq S4 object
#' @param res A DESeq result dataframe
#' @param alpha A number for the cutoff of the adjusted p values
#' @return A dataframe with the combined results and taxa
#' @export
#' @examples
#' result2taxa(phy=phy0,res=results,alpha=0.1)
result2taxa<-function(phy,res, alpha){
  sigtab<-res[(res$padj < alpha), ]
  sigtab = cbind(as(sigtab, "data.frame"), as(tax_table(phy)[rownames(sigtab), ], "matrix"))
  (sigtab)
  return(sigtab)
}