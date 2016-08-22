#' Make phyloseq S4 object
#' 
#' @param workdir Location of files
#' @param biom The biom file name
#' @param tre The tree file (midpoint rooting)
#' @param rep.set The representative sequence file (rep_set1.fna)
#' @param map The metadata map (change '#SampleID' to 'SampleID')
#' @return An S4 phyloseq object
#' @export
#' @examples
#' make_phyloseq(setdir,biom.file,rep_set_tree.file,rep_set.fna,map_file)
make_phyloseq<-function(workdir,biom,tre,rep.set,map){
  Exvivobiom=paste(workdir,biom,sep="")
  Exvivotree=paste(workdir,tre,sep="")
  Exvivoref=paste(workdir,rep.set,sep="")
  Exvivomap=paste(workdir,map,sep="")
  exbiom<-import_biom(Exvivobiom, parseFunction=parse_taxonomy_default)
  exmap<-import_qiime_sample_data(Exvivomap)
  extree<-read_tree(Exvivotree)
  exvivophylo<-merge_phyloseq(exbiom,exmap,extree)
  tax_table(exvivophylo)<-as.matrix(tax_table(exvivophylo)[,1:6])
  colnames(tax_table(exvivophylo)) = c("Domain", "Phylum", "Class", "Order", "Family", "Genus")
  return(exvivophylo)
}