#' plotProp function
#' @description This function takes input of a phyloseq object, the x and y groups, and the group to use for the fill and returns a  proportion table
#' @param phy a Phyloseq S4 object
#' @param x The X variable to use
#' @param y The y variable to use
#' @param fill The category to use for making the proportions 
#' @param n The number of top taxa in the fill category to return   
#' @examples
#' pp<-plotProp(phy1,x="Subject", y="Abundance", fill="Genus", n=10)
fnct<-function(x){x*100/sum(x)}

plotProp<-function(phy,x,y,fill="Phylum",n=10)
  {
  p<-psmelt(phy)
  p.tax<-unique(p[,fill])
  rs<-head(p.tax, n)
  R2 <- ddply(p, x, transform, Pcnt=fnct(get(eval(y))))
  R2<-R2[order(R2[,fill]),]
  R2<-R2[(R2$Pcnt!=0 & R2[,fill]%in%rs),]
  return(R2)
  }

