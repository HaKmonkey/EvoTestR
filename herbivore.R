# return a new herbivore (list)
# use the ID (output[[1]]) to hold the position in the herbivore matrix
# get and manipulate information from the .csv by ID (row)
new.herbivore <- function(herbivore.log, ID, health, age, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20){
  x <- matrix(c(ID, health, age, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20), nrow = 1, ncol = 23)
  herbivore.log <- rbind(herbivore.log, x)
  return(herbivore.log)
}


herbivore.log <- new.herbivore(herbivore.log, 1, 100, 50, 'A', 'A', 'A', 'A', 'A', 'T', 'T', 'T', 'T', 'T', 'G', 'G', 'G', 'G', 'G', 'C', 'C', 'C', 'C', 'C')


temp <- herbivore.log[which(herbivore.log[, 'ID'] == 1), ] # replace number with i in a loop
temp['age'] ## example
temp['health'] ## example
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
herbivore.timestep <- function(herbivores, plants, terrain, herbivore.log, ID, kill, herbivore.health, herbivore.age, herbivore.repro){
    kill <- function(plants, row, col){
    	if(runif(1) <= kill)
        	plants[row, col] <- ""
        return(plants)
    }

    reproduce.herbivore <- function(herbivores, row, col, herbivore.log, herbivore.health, herbivore.age, herbivore.repro, ID){
      if(runif(1) <= herbivore.repro){
      	new.location <- new.loc.herb(herbivores, row, col)
        temp <- herbivore.log[which(herbivore.log[, 'ID'] == ID), ]
        ID <- ID + 1
      	herbivores[new.location[1], new.location[2]] <- ID
        # will this even work? may have to return the herbivore.log as well and adjust how information is accessed in evotest.R
        herbivore.log <- new.herbivore(ID, herbivore.health, herbivore.age, temp['b1'], temp['b2'], temp['b3'], temp['b4'], temp['b5'], temp['b6'], temp['b7'], temp['b8'], temp['b9'], temp['b10'], temp['b11'], temp['b12'], temp['b13'], temp['b14'], temp['b15'], temp['b16'], temp['b17'], temp['b18'], temp['b19'], temp['b20'])
      	return(herbivores)
      }
    }

    ## modifying eat function so that it is based on health
    eat <- function(herbivores, plants, row, col, kill, herbivore.log, ID){
      temp <- herbivore.log[which(herbivore.log[, 'ID'] == ID), ]
      prob <- c(1 - as.int(temp['health'])/100, as.int(temp['health'])/100)
      if(sample(c(TRUE, FALSE), 1, replace = FALSE, prob = prob)){
      	## herbivores[row, col] <- 5
        herb.id <- herbivores[r, c]
        ## health is based on plaaaaaaaaants!
        temp2 <- herbivore.log[which(herbivore.log[, 'ID'] == herb.id), ]
      	kill(plants, row, col, kill)
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
        if(herbivores[r, c] != 0 && is.na(herbivores[r, c]) == FALSE){
          if(is.na(plants[r, c]) == FALSE && plants[r, c] != ""){
            herb.id <- herbivores[r, c]
            temp <- herbivore.log[which(herbivore.log[, 'ID'] == herb.id), ]
            if(as.int(temp['health']) >= 50){
              reproduce.herbivore(herbivores, r, c, herbivore.log, herbivore.health, herbivore.age, herbivore.repro, ID)
            }
            eat(herbivores, plants, r, c, info.herb)
          }else{
            move(herbivores, r, c)
          }
        }
      }
    }
    return(list(herbivores, plants))
  }
