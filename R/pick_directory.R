#' A function to choose the working directory
#' 
#' @return A string with the working directory
#' @export
#' @examples 
#' pick_directory()
pick_directory<-function(){
  dir<-tk_choose.dir()
  dir<-paste(dir,"/",sep="")
  return(dir)
}