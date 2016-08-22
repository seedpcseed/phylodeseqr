#' Performs intersection and difference calculations for a list of vectors. 
#' Performs Fisher exact tests.
#' @param x A list of vectors to compare.
#' @examples
#' findIntersections(x)  
findIntersections<-function(x){
  if(length(x)<2 | length(x)>5){
    stop("Too many comparisons. \n Keep between 2 and 5.")
  }
  
  list.names <- names(x)
  
  if(2==length(x)){
    A <- x[[1]]
    B <- x[[2]]
    n12 <- intersect(A, B)
    area=list(a1=length(A),a2=length(B),
              areaTotal=sum(length(A),length(B)))
    int.list <- list(area=area,
                     n12 = n12,   
                     category = list.names 
    )
  }
  else if(3==length(x)){
    A <- x[[1]]
    B <- x[[2]]
    C <- x[[3]]
    n12 <- intersect(A, B)
    n13 <- intersect(A, C)
    n23 <- intersect(B, C)
    n123 <- intersect(n12, C)
    area=list(a1=length(A),a2=length(B),a3=length(c),
              areaTotal=sum(length(A),length(B),length(C)))
    int.list <- list(area=area,
                     n12 = n12, n13 = n13,  
                     n23 = n23, 
                     n123 = n123,  
                     category = list.names 
    )
  }
  else if(4==length(x)){
    A <- x[[1]]
    B <- x[[2]]
    C <- x[[3]]
    D <- x[[4]]
    n12 <- intersect(A, B)
    n13 <- intersect(A, C)
    n14 <- intersect(A, D)
    n23 <- intersect(B, C)
    n24 <- intersect(B, D)
    n34 <- intersect(C, D)
    n123 <- intersect(n12, C)
    n124 <- intersect(n12, D)
    n134 <- intersect(n13, D)
    n234 <- intersect(n23, D)
    n1234 <- intersect(n123, D)
    area=list(a1=length(A),a2=length(B),a3=length(c),a4=length(D),
              areaTotal=sum(length(A),length(B),length(C),length(D)))
    int.list <- list(area=area,
                     n12 = n12, n13 = n13, n14 = n14, 
                     n23 = n23, n24 = n24, n34 = n34, 
                     n123 = n123, n124 = n124, n134 = n134, 
                     n234 = n234, n1234 = n1234, category = list.names 
                      )
  }
  else if(5==length(x)){
    A <- x[[1]]
    B <- x[[2]]
    C <- x[[3]]
    D <- x[[4]]
    E <- x[[5]]
    n12 <- intersect(names(A), names(B))
    n13 <- intersect(names(A),names(C))
    n14 <- intersect(names(A),names(D))
    n15 <- intersect(names(A),names(E))
    n23 <- intersect(names(B),names(C))
    n24 <- intersect(names(B),names(D))
    n25 <- intersect(names(B), names(E))
    n34 <- intersect(names(C),names(D))
    n35 <- intersect(names(C), names(E))
    n45 <- intersect(names(D),names(E))
    n123 <- intersect(n12, names(C))
    n124 <- intersect(n12, names(D))
    n125 <- intersect(n12, names(E))
    n134 <- intersect(n13, names(D))
    n135 <- intersect(n13, names(E))
    n145 <- intersect(n14, names(E))
    n234 <- intersect(n23, names(D))
    n235 <- intersect(n23, names(E))
    n245 <- intersect(n24, names(E))
    n345 <- intersect(n34, names(E))
    n1234 <- intersect(n123, names(D))
    n1235 <- intersect(n123, names(E))
    n1245 <- intersect(n124, names(E))
    n1345 <- intersect(n134, names(E))
    n2345 <- intersect(n234, names(E))
    n12345 <- intersect(n1234, names(E))
    area=list(a1=length(A),a2=length(B),a3=length(c),a4=length(D),a5=length(E),
              areaTotal=sum(length(A),length(B),length(C),length(D),length(E)))
    int.list <- list(area=area,
                     n12 = n12, n13 = n13, n14=n14,n15=n15,n24=n24,
                     n25=n25, n34=34, n35=n35, n45=n45, n123=n123, 
                     n124 = n124, n125 = n125, n134 = n134, 
                     n135 = n135, n145 = n145, n234 = n234, 
                     n235 = n235, n245 = n245, n345 = n235, 
                     n1234 = n1234, n1235 = n1235, n1245 = n1245, 
                     n1345 = n1345, n2345 = n2345, n12345 = n12345, 
                     category = list.names 
                      )
  }

  return(int.list)
}