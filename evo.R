# genetic sequences will be lists of 12 bases 'ATGC'
# seq <- c(c('A','A','A','A','A'), c('T','T','T','T','T'), c('G','G','G','G','G'), c('C','C','C','C','C'))
# x is one of the 4 vectors inside of seq
# the converted value is the 'name' of the enzyme [can make a list of names for the different values from 0 - 15]

# function that converts 5 base sequence into the enzyme (number)
convert <- function(x){
  DNA <- list('A' = 0, 'T' = 1, 'G' = 2, 'C' = 3)
  Enz <- list('0' = 'a', '1' = 'b', '2' = 'c', '3' = 'd', '4' = 'e', '5' = 'f', '6' = 'g', '7' = 'h', '8' = 'i', '9' = 'j', '10' = 'k', '11' = 'l', '12' = 'm', '13' = 'n', '14' = 'o', '15' = 'p')
  temp <- 0
  for(i in 1:length(x))
    temp <- temp + as.integer(DNA[x[i]])
  return(Enz[as.character(temp)])
}
