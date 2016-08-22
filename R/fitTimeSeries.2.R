#' @name fitTimeSeries.2
#' @title Performs smoothing splines anova fitting for time series data.
#' 
#' @details Smoothing-splines ANOVA fit using a melted dataframe with abundances and times.
#' 
#' @param formula Formula to use in the SS ANOVA
#' @param abundance Abundance column name
#' @param lvl Level from which to pull features
#' @param feature Feature of interest within the lvl
#' @param class Class membership (as a factor)
#' @param time Time points vector of relative times (same length as abundance vector)
#' @param id Sample or Subject ID
#' @param include Parameters to include in the SS ANOVA model
#' @return A list
#' @rdname fitTimeSeries.2
#' @export
#' @examples
#' 
#' data(mouseData)
#' om<- objmelt(mouseData)
#' om.ddp<- ddply(om, c("Time", "Group","Genus"), summarise, Abundance=sum(Abundance))
#' fitTimeSeries.2(om.ddp, lvl="Genus",feature="Lactobacillus", class="Group")
#' 
fitTimeSeries.2<-function (melted, abundance="Abundance", lvl="moduleColor", feature, class, time="Time", id, 
                           formula=formula("abundance ~ time * class"), include = c("class", "time:class"), C = 0, B = 1000) 
{
  
  melted.data = melted[melted[,lvl]==feature,]
  class = melted.data[,class]
  time = melted.data[, time]
  id = melted.data[, id]
  abundance= melted.data[,abundance]
  
  
  prep = fitSSAnova(abundance = abundance, class = class, time = time, 
                 id = id, include = include)

  indexPos = ssIntervalCandidate(fit = prep$fit, standardError = prep$se, 
                                 timePoints = prep$timePoints, positive = TRUE, C = C)
  indexNeg = ssIntervalCandidate(fit = prep$fit, standardError = prep$se, 
                                 timePoints = prep$timePoints, positive = FALSE, C = C)
  indexAll = rbind(indexPos, indexNeg)
  if (sum(indexAll[, 1] == indexAll[, 2]) > 0) {
    indexAll = indexAll[-which(indexAll[, 1] == indexAll[, 
                                                         2]), ]
  }
  fit = 2 * prep$fit
  se = 2 * prep$se
  timePoints = prep$timePoints
  fits = data.frame(fit = fit, se = se, timePoints = timePoints)
  #return(list(fits=fits, indexPos=indexPos, indexNeg=indexNeg, indexAll=indexAll))
  if (!is.null(indexAll)) {
    if (length(indexAll) > 0) {
      indexAll = matrix(indexAll, ncol = 4)
      colnames(indexAll) = c("Interval start", "Interval end",
                             "Area", "p.value")
      predArea = cbind(prep$timePoints, (2 * prep$fit))
      permList = ssPerm(prep$data, B = B)
      if (!missing(formula)) {
        permResult = ssPermAnalysis(data = prep$data,
                                    formula = formula, permList = permList, intTimes = indexAll,
                                    timePoints = prep$timePoints, include = include)
      }
      else {
        permResult = ssPermAnalysis(data = prep$data,
                                    permList = permList, intTimes = indexAll, timePoints = prep$timePoints,
                                    include = include)
      }
      for (i in 1:nrow(indexAll)) {
        origArea = predArea[which(predArea[, 1] == indexAll[i,
                                                            1]):which(predArea[, 1] == indexAll[i, 2]),
                            ]
        actArea = trapz(x = origArea[, 1], y = origArea[,
                                                        2])
        indexAll[i, 3] = actArea
        if (actArea > 0) {
          indexAll[i, 4] = 1 - length(which(actArea >
                                              permResult[, i]))/B
        }
        else {
          indexAll[i, 4] = length(which(actArea > permResult[,
                                                             i]))/B
        }
      }
      res = list(timeIntervals = indexAll, data = prep$data,
                 fit = fits, perm = permResult)
      return(res)
    }
  }
  else {
    indexAll = "No statistically significant time intervals detected"
    res = list(timeIntervals = indexAll, data = prep$data,
               fit = fits, perm = NULL)
    return(res)
  }
}