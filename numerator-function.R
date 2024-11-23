numerator_function <- function(j, A, m){
  if(A==1){
    num <- 1 / (m-j+1)
  } else if(A==0){
    num <- (m-j) / (m-j+1)
  }
  return(num)
}
