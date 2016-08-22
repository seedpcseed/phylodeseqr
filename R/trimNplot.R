#'This function will take an S4 metagenomeSeq object and make a proportional barplot
#'from it.
#'@param obj An metagenomeSeq object
#'@param ftc The category for grouping
#'@param category A string with the phylogenetic level to test
#'@param cutoff A number for the proportion cutoff in the plot
#'@param out The object output (matrix vs. MRExperiement object)
#'@param norm TRUE/FALSE for using normalized data, default=TRUE
#'@param log TRUE/FALSE to use log scale, default=FALSE
#'
trimNplot<-function(obj,ftc="Group",category="Genus",cutoff=0,out="matrix", norm=TRUE,log=FALSE){
  obj.cat<-aggSamp(obj,ftc,aggfun=rowSums)
  obj.cat=aggTax(obj.cat, lvl = category, out = out, norm=norm,log=log)
  obj.cat<-prop.table(obj.cat,2)
  library(reshape2)
  genus.melt<-melt(obj.cat)
  colnames(genus.melt)<-c(category,ftc,"Proportion")
  #   index<-match(genus.melt$Sample,sample_data(phy)$X.SampleID)
  #   genus.melt<-data.frame(Group=sample_data(phy)$Group[index],
  #                          genus.melt)
  #   genus.melt<-genus.melt[genus.melt$Proportion!=0,]
  #   formula=as.formula(paste("Proportion ~ Group +",category))
  #   genus.melt<-aggregate(formula,data=genus.melt, FUN=mean)
  #   summary(genus.melt)
  
  genus.melt.1<-subset(genus.melt,Proportion>cutoff)
  genus.melt.1[order(-genus.melt.1$Proportion,genus.melt.1[,category]),]
  
  library(RColorBrewer)
  colourCount = length(unique(genus.melt.1[,category]))
  getPalette = colorRampPalette(brewer.pal(9, "Paired"))
  values = getPalette(colourCount)
  values.rnd<-sample(values,length(values),length(values))
  
  p<-ggplot(genus.melt.1,aes_string(x=ftc,y="Proportion",fill=category))
  p<-p+geom_bar(stat="identity",aes_string(Group=ftc))+theme_bw()+
    guides(fill=guide_legend(ncol=2))+scale_fill_manual(values = values)
  p
}