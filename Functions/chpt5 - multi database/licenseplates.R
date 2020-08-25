licenseplates <- function(N){ 
  
  let <- vector(length = 125)
  for (i in 1:125) {
    let[i] <- paste(permutations(n = 5, r = 3, v = letters[1:5], repeats.allowed = TRUE)[i,], sep="", collapse="")
  }
  
  ## UNIQUE NUMBER COMBINATIONS
  
  num <- vector(length = 10^3)
  for(j in 1:10^3){
    num[j]  <- paste(permutations(n = 10, r = 3, v = 0:9, repeats.allowed = TRUE)[j,], sep="", collapse="")
  }
  
  # put lets and nums together in all possible combinations 
  result <- paste(rep(let[1:125], times=10), rep(num[1:10], times=125), sep=" ")
  plates <- data.frame( ID = 1:125000, PlateID = result)
  
  # sample for db
  licenseDB <- sample_n(plates, N)
  licenseDB[,1] <- c(1:N)
  
  return(licenseDB)
}