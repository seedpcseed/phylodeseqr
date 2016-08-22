#' Plot diversity data from the make_richness function
#' 
#' @param MR A dataframe with the richness data from Make_richness
#' @param category A character string in quotes for what category to use for grouping.
#' @param color A string with information about the boxplot color
#' @return A boxplot of the richness data
#' @export
#' @examples
#' richness_plot(MR, color="light green")
#' 
richness_plot<-function(MR,category="Group",color="light green")
{
  theme_bw()
  ggplot(MR,aes_string(x=category, y="value"))+geom_boxplot(fill=color,position="dodge")+
    geom_point(position="jitter", size=3)+
    facet_wrap(~variable, scales="free_y") + geom_text(label=aes(value))+
    theme(axis.text.x=element_text(angle=90, size=12)) + ylab("Index") +
    theme(axis.text.y=element_text(size=12))+
    theme(panel.grid = element_blank())
}
