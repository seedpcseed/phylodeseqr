#' Convert dataframe objects to biom-format \code{biom-class} objects.
#'
#' Wrapper to convert dataframe objects to \code{biom-class} objects. Currently cannot
#' create sparse biom-format objects. The user can then export their previously
#' prepared files using \code{write_biom}.
#' \href{http://biom-format.org/documentation/biom_format.html}{the
#' biom-format definition}.
#'
#' #' The BIOM file format (canonically pronounced biome) is designed to be a general-use format for representing biological sample by observation contingency tables. BIOM is a recognized standard for the \href{http://www.earthmicrobiome.org/}{Earth Microbiome Project} and is a \href{http://gensc.org/}{Genomics Standards Consortium} candidate project. Please see \href{http://biom-format.org/}{the biom-format home page} for more details.
#'
#' @param data (Required) Dataframe or matrix of count data.
#' @param sample_metadata Dataframe of sample metadata.
#' @param observation_metadata Dataframe of feature metadata.
#' @param id Optional identifier for the project.
#' @return An object of \code{biom-class}.
#'
#' @references \url{http://biom-format.org/}
#'
#' @examples
#' # randomly create a count matrix
#' set.seed(101)
#' counts = matrix(sample(100,10*10),10,10)
#' rownames(counts) = paste("feature",1:10,sep="")
#' colnames(counts) = paste("sample",1:10,sep="")
#' # create a sample_metadata df
#' class = sample(0:1,10,replace=TRUE)
#' names(class) = colnames(counts)
#' class = as.data.frame(class)
#' # create a observation_metadata df
#' features = sample(c("bacteria_a","bacteria_b","bacteria_c"),10,replace = TRUE)
#' names(features) = rownames(counts)
#' features = as.data.frame(features)
#'
#' biom_obj = convert_to_biom(counts,sample_metadata=class,observation_metadata=feature_meta)
#' # write(biom_obj,output)
convert_to_biom <- function(data,sample_metadata=NULL,observation_metadata=NULL,id=NULL){
  id = id
  format = "Biological Observation Matrix 1.0.0-dev"
  format_url = "http://biom-format.org/documentation/format_versions/biom-1.0.html"
  type = "OTU table"
  generated_by = sprintf("biom %s",packageVersion("biom"))
  matrix_type = "dense"
  matrix_element_type = "int"
  date = as.character(Sys.time())
  shape = dim(data)
  
  if(!is.null(observation_metadata)){
    obs_names = colnames(observation_metadata)
    rows = lapply(1:nrow(data),function(i){ll = list(
      id=rownames(data)[i],
      metadata=lapply(1:ncol(observation_metadata),function(j){
        as.character(observation_metadata[i,j])}))
      names(ll$metadata) = obs_names
      ll
    })
    
  } else {
    rows = lapply(1:nrow(data),function(i){list(
      id=rownames(data)[i],
      metadata=NA)})
  }
  if(!is.null(sample_metadata)){
    sample_names = colnames(sample_metadata)
    columns  = lapply(1:ncol(data),function(i){ll = list(
      id=colnames(data)[i],
      metadata=lapply(1:ncol(sample_metadata),function(j){
        as.character(sample_metadata[i,j])}))
      names(ll$metadata) = sample_names
      ll
    })
    
  } else {
    columns  = lapply(1:ncol(data),function(i){list(
      id=colnames(data)[i],
      metadata=NA)})
  }
  data = as.list(as.data.frame(t(data)))
  names(data) <- NULL
  
  biomlist = list(id=id,format=format,format_url=format_url,type=type,generated_by=generated_by,
                  date=date,matrix_type=matrix_type,matrix_element_type=matrix_element_type,shape=shape,
                  rows=rows,columns=columns,data=data)
  biom(biomlist)
}