#'  Make a DESeq S4 object using a user design and a phyloseq S4 object
#'  
#'  @param phy A phyloseq S4 object
#'  @param design A string
#'  @param design2 A string
#'  @return A DESeq S4 object
#'  @export
#'  @examples
#'  make.deseq(phy0,"Treatment+Time+Treatment:Time","Time")
make_deseq<-function(phy,design, design2="none"){
  if(design2!="none"){
    phydds = phyloseq_to_deseq2(phy, as.formula(design))
    geoMeans = apply(counts(phydds), 1, gm_mean)
    phydds = estimateSizeFactors(phydds , geoMeans = geoMeans)
    phydds <- DESeq(phydds, test=c("LRT"), full=as.formula(design), 
                    reduced=as.formula(design2))
  }
  if(design2=="none"){ 
    phydds = phyloseq_to_deseq2(phy, as.formula(design))
    geoMeans = apply(counts(phydds), 1, gm_mean)
    phydds = estimateSizeFactors(phydds , geoMeans = geoMeans)
    phydds <- DESeq(phydds,fitType="local")  
  } 
  return(phydds)
}