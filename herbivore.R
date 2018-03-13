# return a new herbivore (list)
# use the ID (output[[1]]) to hold the position in the herbivore matrix
# get and manipulate information from the .csv by ID (row)
new.herbivore <- function(ID, health, age, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20){
  output <- list(ID = ID, health = health, age = age, b1 = b1, b2 = b2, b3 = b3, b4 = b4, b5 = b5, b6 = b6, b7 = b7, b8 = b8, b9 = b9, b10 = b10, b11 = b11, b12 = b12, b13 = b13, b14 = b14, b15 = b15, b16 = b16, b17 = b17, b18 = b18, b19 = b19, b20 = b20)
  x <- as.data.frame(output)
  write.table(x, file = "herbivore_log.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  class(output)
  return(output)
}
# ==========================================================================================================
new.loc.herb <- function(mat, row, col){
  possible.location <- as.matrix(expand.grid(row + c(-1, 0, 1), col + c(-1, 0, 1)))

  # get rid of the points outside of the matrix
  out <- numeric()
  for(i in 1:nrow(possible.location)){
    if(possible.location[i, 1] > nrow(mat) || possible.location[i, 2] > nrow(mat) || possible.location[i, 1] == 0 || possible.location[i, 2] == 0)
      out <- c(out, i)
  }

  if(length(out) != 0)
    possible.location <- possible.location[- out, ]

  # get rid of locations that are water
  water <- numeric()
  for(i in 1:nrow(possible.location)){
    if(is.na(mat[possible.location[i, 1], possible.location[i, 2]]))
      water <- c(water, i)
  }

  if(length(water) != 0)
    possible.location <- possible.location[- water, ]

  # if there is only the origin left exit reproduce function
  if(class(possible.location) != "matrix")
    return(possible.location <- c(possible.location, row, col))

  herbs <- numeric()
  for(i in 1:nrow(possible.location)){
    if(mat[possible.location[i, 1], possible.location[i, 2]] != 0)
      herbs <- c(herbs, i)
  }

  if(length(herbs) != 0)
    possible.location <- possible.location[- herbs, ]

  # if there is only the origin left exit reproduce function **
  if(class(possible.location) != "matrix")
    return(possible.location <- c(possible.location, row, col))

  # randomly select which location to reproduce in
  new.location <- numeric()
  if(class(possible.location) != "matrix"){
    return(new.location <- c(new.location, possible.location[1], possible.location[2]))
  }else{
    random.row <- sample(seq(1,nrow(possible.location)), 1)
    return(new.location <- c(new.location, possible.location[random.row, 1], possible.location[random.row, 2]))
  }
}
# ==========================================================================================================
herbivore.timestep <- function(herbivores, plants, terrain, herbivore.health, herbivore.age, herbivore.frac, herbivore.repro, kill, ID){
    kill <- function(plants, row, col){
    	if(runif(1) <= kill)
        	plants[row, col] <- ""
        return(plants)
    }

    reproduce.herbivore <- function(herbivores, row, col){
      if(runif(1) <= herbivore.repro){
      	new.location <- new.loc.herb(herbivores, row, col)
      	herbivores[new.location[1], new.location[2]] <- ID
        new.herbivore(ID, herbivore.health, herbivore.age, ) # have to find a way to make bases heritable
        ID <- ID + 1
      	return(herbivores)
      }
    }

    # have to figure out how to pull herbivore health from log file (might make a matrix with the information)
    eat <- function(herbivores, plants, row, col, ){
      p <- which(info.herb$herbivore.state == herbivores[row, col])
      prob.eat <- c(info.herb$eat[p], 1 - info.herb$eat[p])
      if(sample(c(TRUE, FALSE), 1, replace = FALSE, prob = prob.eat)){
      	herbivores[row, col] <- 5
      	kill(plants, row, col, info.herb$kill)
      	reproduce.herbivore(herbivores, row, col)
      }else{
      	move(herbivores, row, col)
      }
      return(list(herbivores, plants))
    }

    move <- function(herbivores, row, col){
      new.location <- new.loc.herb(herbivores, row, col)
      herbivores[new.location[1], new.location[2]] <- herbivores[row, col]
      herbivores[row, col] <- 0
      return(herbivores)
    }

    for(r in 1:nrow(terrain)){
      for(c in 1:ncol(terrain)){
        if(herbivores[r, c] != 0 && is.na(herbivores[r,c]) == FALSE){
          if(is.na(plants[r, c]) == FALSE && plants[r, c] != ""){
            eat(herbivores, plants, r, c, info.herb)
          }else{
            move(herbivores, r, c)
          }
        }
      }
    }
    return(list(herbivores, plants))
  }
