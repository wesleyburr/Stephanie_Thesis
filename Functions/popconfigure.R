## Chpt 5.3 - implementing allele distributions

popconfigure <- function(PopSource){
  
  names(PopSource) <- PopSource[1, ]
  PopSource <- PopSource[-1, ]
  for(j in seq(2, 26, 2)) {
    PopSource[, j] <- as.numeric(PopSource[, j])
    PopSource[, j] <- PopSource[, j] / sum(PopSource[, j], na.rm = TRUE)
  }
  return(PopSource)
}
