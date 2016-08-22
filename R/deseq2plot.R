#' Make a plot from deseq results
#' 
#' @param sigtab A dataframe with results from DESeq
#' @param tax A string with the taxon level to show
#' @return A plot of the sigtab data
#' @export
#' @examples
#' deseq2plot(sigtab=sigtab1,tax="Genus")
deseq2plot<-function(sigtab, tax){
  print(paste("Analyzing for ",tax," using sigtab",sep=""))
  scale_fill_discrete <- function(palname = "Set1", ...) {
    scale_fill_brewer(palette = palname, ...)}
  sigtabgen = sigtab[!is.na(sigtab[,tax]),]#subset(sigtab, !is.na(sigtab[,tax]))
  # Phylum order
  x = tapply(sigtabgen$log2FoldChange, sigtabgen$Phylum, function(x) max(x))
  x = sort(x, TRUE)
  sigtabgen$Phylum = factor(as.character(sigtabgen$Phylum), levels=names(x))
  # Genus order
  x = tapply(sigtabgen$log2FoldChange, sigtabgen[,tax], function(x) max(x))
  x = sort(x, TRUE)
  sigtabgen[,tax] = factor(as.character(sigtabgen[,tax]), levels=names(x))
  sigtabgen<-sigtabgen[order(sigtabgen$padj),]
  print(head(sigtabgen))
  print(tail(sigtabgen))
  
  ggplot(sigtabgen, aes_string(x=tax, y="log2FoldChange", color="Phylum")) + geom_point(size=1.5, color="black")+ 
    geom_point(size=5, alpha=.7) + 
    theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0.5, size=12),
          axis.text.y=element_text(size=14),legend.text=element_text(size=12),
          axis.title.x=element_text(size=18),axis.title.y=element_text(size=18),
          legend.title=element_text(size=16))+
    geom_hline(aes(yintercept=0.5), colour="black", linetype="dashed", size=.8)+
    geom_hline(aes(yintercept=-0.5), colour="black", linetype="dashed",size=.8)+
    geom_hline(aes(yintercept=-0), colour="black", size=.8)+
    theme(panel.background=element_rect(size=2,colour="black"),
          panel.grid.major=element_line(colour="grey",size=.2)
          #panel.grid.minor=element_line(colour="grey")
    )
}