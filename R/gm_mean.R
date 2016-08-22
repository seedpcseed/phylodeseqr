#' Calculate the geometric mean
#' 
#' @param x A numeric vector
#' @param na.rm Set to default TRUE
#' @return The geometric mean of the vector
#' @export
#' @examples
#' gm_mean(list.x,na.rm=TRUE)
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}