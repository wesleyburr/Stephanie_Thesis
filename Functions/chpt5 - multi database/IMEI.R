IMEI <- function(N){
  # Generate a population size N 
  phones <- data.frame(ID = 1:5^5, phoneID = NA)
  
  im <- vector(length = 5^5)
  
  for(i in 1:5^5) {
    im[i] <- paste(permutations(n = 5, r = 5, v = 0:4, repeats.allowed = TRUE)[i, ], sep="", collapse="")
  }
  
  phones[, 2] <- data.frame(im)
  
  
  imeiDB <-sample_n(phones, N)
  imeiDB[,1] <- c(1:N)
  
  return(imeiDB)
}