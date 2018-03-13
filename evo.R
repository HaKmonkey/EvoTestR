# genetic sequences will be lists of 12 bases 'ATGC'
# seq <- c(c('A','A','A','A','A'), c('T','T','T','T','T'), c('G','G','G','G','G'), c('C','C','C','C','C'))
# x is one of the 4 vectors inside of seq
# the converted value is the 'name' of the enzyme [can make a list of names for the different values from 0 - 15]

# function that converts 5 base sequence into the enzyme (number)
to.enz <- function(x){
  DNA <- list('A' = 0, 'T' = 1, 'G' = 2, 'C' = 3)
  Enz <- list('0' = 'a', '1' = 'b', '2' = 'c', '3' = 'd', '4' = 'e', '5' = 'f', '6' = 'g', '7' = 'h', '8' = 'i', '9' = 'j', '10' = 'k', '11' = 'l', '12' = 'm', '13' = 'n', '14' = 'o', '15' = 'p')
  temp <- 0
  for(i in 1:length(x))
    temp <- temp + as.integer(DNA[x[i]])
  return(Enz[as.character(temp)])
}

to.dna <- function(x, possbible.enz){
  Enz <- list('a' = 0, 'b' = 1, 'c' = 2, 'd' = 3, 'e' = 4, 'f' = 5, 'g' = 6, 'h' = 7, 'i' = 8, 'j' = 9, 'k' = 10, 'l' = 11, 'm' = 12, 'n' = 13, 'o' = 14, 'p' = 15)
  DNA <- list('A', 'T', 'G', 'C')
  dna.list <- as.matrix(expand.grid(DNA, DNA, DNA, DNA, DNA))
  for(i in 1:nrow(dna.list)){
    temp <- 1
    # how to exit after the bases have been selected
    if(temp == 5){
      return bases
    }
    if(as.character(to.enz(dna.list[1,])) %in% possible.enz){
      bases <- c(bases, dna.list[1, ])
    }
  }
}
