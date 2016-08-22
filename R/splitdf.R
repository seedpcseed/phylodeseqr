#' A function to split a dataframe into parts by random sampling
#' to use in machine learning or Random Forest analysis etc.
#' From: http://www.gettinggeneticsdone.com/2011/02/split-data-frame-into-testing-and.html
#' @param dataframe The dataframe to split
#' @param seed A numeric value for seed reproducibility
#' @return A list containing object$trainset and object$testset
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}