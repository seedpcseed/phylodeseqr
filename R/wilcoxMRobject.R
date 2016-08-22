wilcoxMRobject<-function (obj, otulist,classIndex, log = TRUE, norm = TRUE) 
{
  mat = MRcounts(obj, norm = norm, log = log)
  l = lapply(classIndex, function(j) {mat[otulist, j]})
  y = unlist(l)
  x = rep(seq(along = l), sapply(l, length))
  wilcox.test(y~x)
}