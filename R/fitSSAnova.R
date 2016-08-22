#' @name fitSSAnova
#' @title Performs smoothing splines anova fitting
#' 
#' @details Smoothing-splines ANOVA fit.
#' 
#' @param formula Formula to use in the SS ANOVA
#' @param abundance Abundance data vector
#' @param class Class membership (as a factor)
#' @param time Time points vector of relative times (same length as abundance vector)
#' @param id Sample or Subject ID
#' @param include Parameters to include in the SS ANOVA model
#' @return A list
#' @rdname fitSSAnova
#' @export
fitSSAnova<-function (formula, abundance, class, time, id, include = c("class", 
                                                           "time:class")) 
{
  df = data.frame(abundance = abundance, class = factor(class), 
                  time = time, id = factor(id))
  if (missing(formula)) {
    mod = gss::ssanova(abundance ~ time * class, data = df)
  }
  else {
    mod = gss::ssanova(formula, data = df)
  }
  fullTime = seq(min(df$time), max(df$time), by = 1)
  values = data.frame(time = fullTime, class = factor(levels(df[, "class"]))[2])
  fit = predict(mod, values, include = include, se = TRUE)
  res = list(data = df, fit = fit$fit, se = fit$se, timePoints = fullTime)
  return(res)
}