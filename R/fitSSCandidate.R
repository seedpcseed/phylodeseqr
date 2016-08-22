fitSSCandidate<- function (fit, standardError, timePoints, positive = TRUE, C = 0) 
{
  lowerCI = (2 * fit - (1.96 * 2 * standardError))
  upperCI = (2 * fit + (1.96 * 2 * standardError))
  if (positive) {
    abundanceDifference = which(lowerCI >= 0 & abs(lowerCI) >= C)
  }   else {
    abundanceDifference = which(upperCI <= 0 & abs(upperCI) >= C)
  }
  if (length(abundanceDifference) > 0) {
    intIndex = which(diff(abundanceDifference) != 1)
    intTime = matrix(NA, (length(intIndex) + 1), 4)
    if (length(intIndex) == 0) {
      intTime[1, 1] = timePoints[abundanceDifference[1]]
      intTime[1, 2] = timePoints[tail(abundanceDifference, 
                                      n = 1)]
    }
    else {
      i = 1
      while (length(intTime) != 0 & length(intIndex) != 
             0) {
        intTime[i, 1] = timePoints[abundanceDifference[1]]
        intTime[i, 2] = timePoints[abundanceDifference[intIndex[1]]]
        abundanceDifference = abundanceDifference[-c(1:intIndex[1])]
        intIndex = intIndex[-1]
        i = i + 1
      }
      intTime[i, 1] = timePoints[abundanceDifference[1]]
      intTime[i, 2] = timePoints[tail(abundanceDifference, 
                                      n = 1)]
    }
  }
  else {
    intTime = NULL
  }
  return(intTime)
}