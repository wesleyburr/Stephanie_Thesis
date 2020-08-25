creditcard <- function(N){
  # Generate a population size N 
  cards <- data.frame(ID = 1:5^5, CardID = NA)
  
  credit <- vector(length = 5^5)
  
  for(i in 1:5^5) {
    credit[i] <- paste(permutations(n = 5, r = 5, v = 0:4, repeats.allowed = TRUE)[i, ], sep="", collapse="")
  }
  
  cards[, 2] <- data.frame(credit)
  
  cardsDB <- sample_n(cards, N)
  cardsDB[,1] <- c(1:N)

  return(cardsDB)
}