# This is a function that creates terrain with varied heights.
# A user set perameter 'water' determines the lowest qantile of 
# terrain that becomes 'NA'.A value of 'NA' becomes water.
terrain <- function(n = 6 , water = .2, noise = c(5, 5)){
  print('terrain')

  # 'make.terrain' is making the matrix for the terrain and assigning the 4 corners.
	make.terrain <- function(n, sd = noise[1]){
		env <- matrix (NA, nrow = 2 ^ n + 1, ncol = 2 ^ n + 1)
		env[1,1] <- rnorm(1, 0, sd)
		env[1,ncol(env)] <- rnorm(1, 0, sd)
		env[ncol(env),1] <- rnorm(1, 0, sd)
		env[ncol(env),ncol(env)] <- rnorm(1, 0, sd)
		return(env)
	}

  # 'diamond.step' is finding the center of the matrix and giving it a value - 
  # the average of the four corners. To add noise to the terrain generation we 
  # change the standard deviation.
	diamond.step <- function(env, sd = noise[2]){
		center <- mean(c(env[1,1], env[1,ncol(env)], env[ncol(env),1], env[ncol(env),ncol(env)]))
		center <- rnorm(1, center, sd)
		env[ceiling(ncol(env) / 2), ceiling(ncol(env) / 2)] <- center
		return(env)
	}

  # 'square.step' is finding the 4 points that corospond with the 
  # corners of each matrix and giving them values.
	square.step <- function(env, sd = noise[2]){
		center <- env[ceiling((ncol(env) / 2)), ceiling((ncol(env) / 2))]
		env[1, ceiling(ncol(env) / 2)] <- rnorm(1, mean(c(env[1,1], env[1,ncol(env)], center)), sd)
		env[ceiling(ncol(env) / 2), ncol(env)] <- rnorm(1, mean(c(env[1,ncol(env)], env[ncol(env),ncol(env)], center)), sd)
		env[ncol(env), ceiling(ncol(env) / 2)] <- rnorm(1, mean(c(env[ncol(env),ncol(env)], env[ncol(env), 1], center)), sd)
		env[ceiling(ncol(env) / 2), 1] <- rnorm(1, mean(c(env[1,1], env[ncol(env),ncol(env)], center)), sd)
		return(env)
	}

	env <- make.terrain(n)

  # Loop through the subseted matricies and apply the 'diamond.step' 
  # and then the 'square.step' to all. '2 ^ (n:1)' gives an 'i' value to 
  # help subset the matrix into succesively smaller matricies. 's' is a 
  # temporary value that becomes a sequence that will sub-setted for matrix coordinates.

  # If n <- 4 your matrix will be 2^n+1 or 17x17.
  # The first iteration of this loop 'i' will be 16, then 8, then 4, then 2.
  # The 'object' 's' will then be the following sequences: 
  # (1,17), (1,9,17), (1,5,9,13,17), & (1,3,5,7,9,11,13,15,17). 
	for(i in 2 ^ (n:1)){
		s <- seq(1,ncol(env), by = i)
		for(r in 1:(length(s) - 1)){
			for(c in 1:(length(s) - 1)){
				env[s[r]:s[r + 1], s[c]:s[c + 1]] <- diamond.step(env[s[r]:s[r + 1], s[c]:s[c + 1]])
			}
		}
		for(r in 1:(length(s) - 1)){
			for(c in 1:(length(s) - 1)){
				env[s[r]:s[r + 1], s[c]:s[c + 1]] <- square.step(env[s[r]:s[r + 1], s[c]:s[c + 1]])
			}
		}
	}

	# 'replace' is changeing the selected quantile on the matrix to NA (water).
	env <- replace(env, env <= quantile(env, probs = water) , NA)

	# This is creating a second matrix that represents the presence of water.
	water <- matrix(0, nrow = nrow(env), ncol = ncol(env))
	water <- replace(water, is.na(env), 1)

	# The 'image' functions image both the terrain and the water.
	# image(env, col = terrain.colors(length(env)))
	# image(water, col = c(NA, "blue"), add = TRUE)

	return(env)
}

terrain <- terrain() # example

# END OF TERRAIN

# Creates a plant with 3 enzymes and 3 values that are associated with the enzymes. 
# These values will be the amount of 'health' that is returned to the herbivore 
# if it has that specific enzyme. A constant value will be added later 
# (in a different function) that removes a set amount of health from a herbivore 
# if it eats a plant without the propper enzyme.
new.plant <- function(enz1, enz2, enz3, val1 = 25, val2 = 12.5, val3 = 6.25){
  print('new.plant')
  output <- matrix(c(enz1, enz2, enz3, val1, val2, val3), nrow = 3, ncol = 2)
	return(output)
}

# END OF NEW.PLANT

# This function determines placement of the plants based on a quantile of terrain height.
# There are currently 5 quantiles because there are 5 plants.
new.loc.plant <- function(plants, row, col, terrain){
  print('new.loc.plant')
  possible.location <- as.matrix(expand.grid(row + c(-1, 0, 1), col + c(-1, 0, 1)))

  new.location <- numeric() # variable moved here because it is used at the end, and to help escape for terrain

  # Get rid of the points outside of the 'terrain' matrix.
  out <- numeric()
  for(i in 1:nrow(possible.location)){
    if(possible.location[i, 1] > nrow(plants) || possible.location[i, 2] > nrow(plants) || possible.location[i, 1] == 0 || possible.location[i, 2] == 0 && class(possible.location) == "matrix")
      out <- c(out, i)
  }

  if(length(out) != 0)
    possible.location <- possible.location[- out, ]

  # Get rid of locations that are water (NA).
  water <- numeric()
  for(i in 1:nrow(possible.location)){
    if(is.na(plants[possible.location[i, 1], possible.location[i, 2]]) && class(possible.location) == "matrix")
      water <- c(water, i)
  }

  if(length(water) != 0)
    possible.location <- possible.location[- water, ]

  # 'quant.*': quantiles for terrain height that constrain plant placement.
  quant.p1 <- quantile(terrain, probs = .22, na.rm = TRUE)
  quant.p2 <- quantile(terrain, probs = seq(.2, .42), na.rm = TRUE)
  quant.p3 <- quantile(terrain, probs = seq(.4, .62), na.rm = TRUE)
  quant.p4 <- quantile(terrain, probs = seq(.6, .82), na.rm = TRUE)
  quant.p5 <- quantile(terrain, probs = seq(.8, 1), na.rm = TRUE)
  
  # Get rid of values outside of the height limit for each plant.
  height <- numeric()
	if(class(possible.location) != 'matrix'){
		if(plants[row,col] == 'p1' && terrain[possible.location[1], possible.location[2]] <= as.numeric(quant.p1))
      return(possible.location <- c(possible.location, row, col))
    if(plants[row,col] == 'p2sample' && terrain[possible.location[1], possible.location[2]] <= as.numeric(quant.p2))
      return(possible.location <- c(possible.location, row, col))
    if(plants[row,col] == 'p3' && terrain[possible.location[1], possible.location[2]] <= as.numeric(quant.p3))
      return(possible.location <- c(possible.location, row, col))
    if(plants[row,col] == 'p4' && terrain[possible.location[1], possible.location[2]] <= as.numeric(quant.p4))
      return(possible.location <- c(possible.location, row, col))
    if(plants[row,col] == 'p5' && terrain[possible.location[1], possible.location[2]] <= as.numeric(quant.p5))
      return(possible.location <- c(possible.location, row, col))
	} else{
  	for(i in 1:nrow(possible.location)){
    	if(plants[row,col] == 'p1' && class(possible.location) == "matrix" && terrain[possible.location[i, 1], possible.location[i, 2]] <= as.numeric(quant.p1))
      	height <- c(height, i)
    	if(plants[row,col] == 'p2' && class(possible.location) == "matrix" && terrain[possible.location[i, 1], possible.location[i, 2]] <= as.numeric(quant.p2))
      	height <- c(height, i)
    	if(plants[row,col] == 'p3' && class(possible.location) == "matrix" && terrain[possible.location[i, 1], possible.location[i, 2]] <= as.numeric(quant.p3))
        height <- c(height, i)
    	if(plants[row,col] == 'p4' && class(possible.location) == "matrix" && terrain[possible.location[i, 1], possible.location[i, 2]] <= as.numeric(quant.p4))
      	height <- c(height, i)
    	if(plants[row,col] == 'p5' && class(possible.location) == "matrix" && terrain[possible.location[i, 1], possible.location[i, 2]] <= as.numeric(quant.p5))
        height <- c(height, i)
  	}
	}

  if(length(height) != 0){
    possible.location <- possible.location[- height, ]
  }  

  # If there is only the origin left exit reproduce function.
  if(class(possible.location) != "matrix" || length(possible.location) == 0)
    return(possible.location <- c(possible.location, row, col))#

  # Get rid of current origin of plant.
  origin <- numeric()
  for(i in 1: nrow(possible.location)){
    if(possible.location[i, 1] == row && possible.location[i, 2] == col)
      origin <- c(origin, i)
  }
	if(length(origin) != 0){
  	possible.location <- possible.location[- origin, ]
	}

  # Randomly select which location to reproduce in.
  if(class(possible.location) != "matrix"){
    return(new.location <- c(new.location, possible.location[1], possible.location[2]))
  }else{
    random.row <- sample(seq(1,nrow(possible.location)), size = 1)
    return(new.location <- c(new.location, possible.location[random.row, 1], possible.location[random.row, 2]))
  }
}

# END OF NEW.LOC.PLANT

# The 'plant.timestep' function contains all the functions that coincide with
# biological processes that would occur during each generation. 
plant.timestep <- function(plants, terrain){
  print('plant.timestep') 

    # This function determines if a plant in a given cell survives to the 
    # next generation. Currently all plants have an 80% chance of 
    # survival via 'if(runif(1) <= .8) return(cell)'.
    survive <- function(cell){
      print('survive')
      if(is.na(cell))
        return(NA)
      if(cell == "")
        return("")
      if(runif(1) <= .8){
        return(cell)
      }else{
        return(cell <- "")
      }
    }

    # This function allows for two plants to have different competative
    # levels against different plants. Currently all plants have a 
    # 50/50 chance of winning against each other to simplify the model.
    compete <- function(cell_1, cell_2){
      print('compete')
      return(cell_2 <- sample(c(cell_1, cell_2), size = 1))
    }

    # This function checks if a plant is reproducing (asexual) into
    # a new, adjacent, point on the terrain matrix. Currently a plant
    # has a 50/50 chance of reproducing into an adjecnt empty cell. If
    # the cell is not empty, the 'compete' function runs.
    reproduce.plant <- function(row, col, plants, terrain){
      print('reproduce.plant')
      if(runif(1) <= .5){
        print('plant is reproducing')
        new.location <- new.loc.plant(plants, row, col, terrain)
        # This 'if' statement checks for competetion.
        if(plants[new.location[1], new.location[2]] == ""){
          plants[new.location[1], new.location[2]] <- plants[row, col]
        }else{
          compete(plants[row, col], plants[new.location[1], new.location[2]])
        }
      }
      return(plants)
    }

    # This 'for' loop iterates the 'survive' function across all plants.
    for(r in 1:nrow(terrain)){
      for(c in 1:ncol(terrain)){
        plants[r, c] <- survive(plants[r, c])
        if(is.na(plants[r, c]) == FALSE && plants[r, c] != "")
          plants <- reproduce.plant(r, c, plants, terrain)
      }
    }

    return(plants)
  }

# END OF PLANT.TIMESTEP

# This function creates a new herbivore and appends it onto the end of
# the herbivore log matrix. The log file, 'herbivore.log' is instanciated
# in the 'evo.test' function.  
new.herbivore <- function(herbivore.log, ID, health, age, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20){
  print('new.herbivore')
  x <- matrix(c(ID, health, age, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20), nrow = 1, ncol = 23)
  herbivore.log <- rbind(herbivore.log, x)
  ID <- ID + 1
  return(herbivore.log)
}


# herbivore <- new.herbivore(herbivore.log, 1, 100, 50, 'A', 'A', 'A', 'A', 'A', 'T', 'T', 'T', 'T', 'T', 'G', 'G', 'G', 'G', 'G', 'C', 'C', 'C', 'C', 'C') # example
# temp <- herbivore.log[which(herbivore.log[, 'ID'] == 1), ] # replace number with i in a loop
# temp['age'] ## example
# temp['health'] ## example

# END OF NEW.HERBIVORE

# Most of this code is basically directly copied from the 'new.loc.plant'
# function above, with slight differences that account for herbivore behavior.
# Plants do not move out of their "cell" in the matrix. Plants' movement is
# just reproduction. The herbivore must actually leave the space it is in and
# move to a new one.
new.loc.herb <- function(mat, row, col){
  print('new.loc.herb')
  possible.location <- as.matrix(expand.grid(row + c(-1, 0, 1), col + c(-1, 0, 1)))

  # Get rid of the points outside of the 'terrain' matrix
  out <- numeric()
  for(i in 1:nrow(possible.location)){
    if(possible.location[i, 1] > nrow(mat) || possible.location[i, 2] > nrow(mat) || possible.location[i, 1] == 0 || possible.location[i, 2] == 0)
      out <- c(out, i)
  }

  if(length(out) != 0)
    possible.location <- possible.location[- out, ]

  # Get rid of locations that are water (NA).
  water <- numeric()
  for(i in 1:nrow(possible.location)){
    if(is.na(mat[possible.location[i, 1], possible.location[i, 2]]))
      water <- c(water, i)
  }

  if(length(water) != 0)
    possible.location <- possible.location[- water, ]

  # If there is only the origin left exit reproduce function.
  if(class(possible.location) != "matrix")
    return(possible.location <- c(possible.location, row, col))

  herbs <- numeric()
  for(i in 1:nrow(possible.location)){
    if(mat[possible.location[i, 1], possible.location[i, 2]] != 0)
      herbs <- c(herbs, i)
  }

  if(length(herbs) != 0)
    possible.location <- possible.location[- herbs, ]

  # If there is only the origin left exit reproduce function.
  if(class(possible.location) != "matrix")
    return(possible.location <- c(possible.location, row, col))

  # Randomly select which location to reproduce in.
  new.location <- numeric()
  if(class(possible.location) != "matrix"){
    return(new.location <- c(new.location, possible.location[1], possible.location[2]))
  }else{
    random.row <- sample(seq(1,nrow(possible.location)), size = 1)
    return(new.location <- c(new.location, possible.location[random.row, 1], possible.location[random.row, 2]))
  }
}

# END OF NEW.LOC.HERB

# The 'herbivore.timestep' function contains all the functions that coincide with
# biological processes that would occur during each generation. The variables after 
# 'ID' all have defaults in the 'evo.test' function.
herbivore.timestep <- function(herbivores, plants, terrain, herbivore.log, ID, herb.kill, herbivore.health, herbivore.age, herbivore.repro, p1, p2, p3, p4, p5){
  print('herbivore.timestep')

  # This function executes if the herbivore kills a plant. Currently the 
  # chance for killing a plant is set at a static 10%.
  kill <- function(plants, row, col){
    print('kill')
    if(runif(1) <= herb.kill)
      plants[row, col] <- ""
    return(plants)
  }

  # This function executes when a herbivore reproduces. Currently the chance
  # for reproducing is set at a static 50%. Both the 'herbivore.log' and the
  # 'ID' are being updated within the 'new.herbivore' function. 
  reproduce.herbivore <- function(herbivores, row, col, herbivore.log, herbivore.health, herbivore.age,herbivore.repro, ID, herb.id){
    print('reproduce.herbivore')
    if(runif(1) <= herbivore.repro){
      print('herbivore is reproducing')
    	new.location <- new.loc.herb(herbivores, row, col)
      temp <- herbivore.log[which(herbivore.log[, 'ID'] == herb.id), ]
      new.herbivore(ID, herbivore.health, herbivore.age, temp['b1'], temp['b2'], temp['b3'], temp['b4'], temp['b5'], temp['b6'], temp['b7'], temp['b8'], temp['b9'], temp['b10'], temp['b11'], temp['b12'], temp['b13'], temp['b14'], temp['b15'], temp['b16'], temp['b17'], temp['b18'], temp['b19'], temp['b20'])
    	herbivores[new.location[1], new.location[2]] <- (ID-1)
    	return(herbivores)
    }
  }

  ## NOTE TO SELF!
  ## Have to add a check for the propper enzymes to this function
  ## This is where the real work begins O_O

  ## THOUGHT MAP
  ## Have to add a check for the propper enzymes to this function
  ## This is where the real work begins O_O

  ## THOUGHT MAP
  ## As an herbivore eats a plant a check needs to be done compairing
  ## the enzymes that the herbivore makes to the required enzymes of 
  ## the plant.
  ##
  ## $ = a check mark 
  ##
  ## STEP 1 $
  ## Run the 'to.enz' function on the bases that the herbivore makes
  ##
  ## STEP 2 $
  ## Compaire the enzymes to the 3 plant enzymes, which are stored as 
  ## a matrix. (could use a dictionary to associate the health values 
  ## correctly)
  ##
  ## Having to figure out how to pull the data from the plants while
  ## the program is running.
  ##
  ## STEP 3 $
  ## If there is a match, that amount of health is added to the herbivore's
  ## health up to 100, never above. The herbivore can get health back for
  ## every correct enzyme that it has. It does not get health back for an
  ## incorrect enzyme.
  ##
  ## STEP 4 $
  ## If the herbivore does not have any matches ('if') then it will
  ## take a set amount of damage (almost like food poisioning).

  # This function 
  eat <- function(herbivores, plants, row, col, herb.kill, herbivore.log, herb.id, p1, p2, p3, p4, p5){
    print('eat')
    herb <- herbivore.log[which(herbivore.log[, 'ID'] == herb.id), ]
    plant <- plants[row, col]
    prob <- c(1 - as.int(herb['health'])/100, as.int(herb['health'])/100)
    if(sample(c(TRUE, FALSE), 1, replace = FALSE, prob = prob)){

      # This is converting the propper bases to the propper enzymes.
      enz1 <- to.enz(herb['b1'], herb['b2'], herb['b3'], herb['b4'], herb['b5'])
      enz2 <- to.enz(herb['b6'], herb['b7'], herb['b8'], herb['b9'], herb['b10'])
      enz3 <- to.enz(herb['b11'], herb['b12'], herb['b13'], herb['b14'], herb['b15'])
      enz4 <- to.enz(herb['b16'], herb['b17'], herb['b18'], herb['b19'], herb['b20'])

      enz_list <- c(enz1, enz2, enz3, enz4)

      # These conditional statements will help compair the herbivores enzymes
      # to the corretc plant and adjust its health accordingly.
      if(plant == 'p1'){
        n <- which(p1 %in% enz_list)
        if(length(n) != 0){
          for(i in x){
            herbivore.log['health'] <- as.character(as.double(herbivore.log['health']) + as.double(p1[i, 2]))
          }
        }
      } else if(plant == 'p2'){
        n <- which(p2 %in% enz_list)
        if(length(n) != 0){
          for(i in x){
            herbivore.log['health'] <- as.character(as.double(herbivore.log['health']) + as.double(p2[i, 2]))
          }
        }
      } else if(plant == 'p3'){
        n <- which(p3 %in% enz_list)
        if(length(n) != 0){
          for(i in x){
            herbivore.log['health'] <- as.character(as.double(herbivore.log['health']) + as.double(p3[i, 2]))
          }
        }
      } else if(plant == 'p4'){
        n <- which(p4 %in% enz_list)
        if(length(n) != 0){
          for(i in x){
            herbivore.log['health'] <- as.character(as.double(herbivore.log['health']) + as.double(p4[i, 2]))
          }
        }
      } if(plant == 'p5'){
        n <- which(p5 %in% enz_list)
        if(length(n) != 0){
          for(i in x){
            herbivore.log['health'] <- as.character(as.double(herbivore.log['health']) + as.double(p5[i, 2]))
          }
        }
      } else {
          herbivore.log['health'] <- as.character(as.double(herbivore.log['health']) - 10)
      }
      if(as.double(herbivore.log['health']) > 100){
        herbivore.log['health'] <- '100'
      }

      # This is run to see if the herbivore kills the plant while eating.
      kill(plants, row, col, herb.kill)
    }else{
      move(herbivores, row, col)
    }
    return(list(herbivores, plants))
  }

  # This functions executes the 'new.loc.herb' function. This
  # function is acting as a wrapper so that the herbivore is 
  # removed from its origional spot and put into the new spot.
  move <- function(herbivores, row, col){
    print('move')
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
            reproduce.herbivore(herbivores, r, c, herbivore.log, herbivore.health, herbivore.age, herbivore.repro, ID, herb.id)
          }
          eat(herbivores, plants, r, c, herb.kill, herbivore.log, herb.id, p1, p2, p3, p4, p5)
        }else{
          move(herbivores, r, c)
        }
      }
    }
  }
  return(list(herbivores, plants))
}

# END OF HERBIVORE.TIMESTEP

# This function converts 5 bases into a number that is associated
# with the enzyme list and returnes the enzyme as a character.
to.enz <- function(b1, b2, b3, b4, b5){
  print('to.enz')
  x <- list(b1, b2, b3, b4, b5)
  DNA <- list('A' = 0, 'T' = 1, 'G' = 2, 'C' = 3)
  Enz <- list('0' = 'a', '1' = 'b', '2' = 'c', '3' = 'd', '4' = 'e', '5' = 'f', '6' = 'g', '7' = 'h', '8' = 'i', '9' = 'j', '10' = 'k', '11' = 'l', '12' = 'm', '13' = 'n', '14' = 'o', '15' = 'p')
  temp <- 0
  for(i in 1:length(x)){
		base <- x[[i]]
    temp <- temp + DNA[[as.character(base)]]
	}
  return(Enz[as.character(temp)])
}

# END OF TO.ENZ

to.dna <- function(to.use){
  print('to.dna')
  bases <- c()
  DNA <- list('A', 'T', 'G', 'C')
  dna.list <- as.matrix(expand.grid(DNA, DNA, DNA, DNA, DNA))
  temp <- 0
  for(i in 1:nrow(dna.list)){
    enz <- to.enz(dna.list[i, 1], dna.list[i, 2], dna.list[i, 3], dna.list[i, 4], dna.list[i, 5])
    if(temp < 3){
      if(enz %in% to.use){
        bases <- c(bases, dna.list[i, ])
        temp <- temp + 1
      }
    } else {
			return(bases)
		}
  }
}

# END OF TO.DNA (need to fix)

evo.test <- function(terrain, timesteps = 50, herbivore.health = 100, herbivore.age = 50, herbivore.frac = .1, herbivore.repro = .5, herb.kill = .1){
  print('evo.test')

  # This creates the plants array.
  plant.generation <- array(data = "", dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))

  # The plants are randomly assigned the three enzymes that 'digest them'.
  # The order of the enzymes gives the amount of health returned to the herbivore.
  enz <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')

  # This is assigning 3 unique enzymes to each plant. Different plants can 
  # share the same enzymes, but a single plant cannot have the same enzyme
  # more than once.
  temp <- sample(enz, size = 3, replace = FALSE)
  p1 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(temp[1], temp[2], temp[3])

  temp <- sample(enz, size = 3, replace = FALSE)
  p2 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  temp <- sample(enz, size = 3, replace = FALSE)
  p3 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  temp <- sample(enz, size = 3, replace = FALSE)
  p4 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  temp <- sample(enz, size = 3, replace = FALSE)
  p5 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  # This is creating count values for enzymes that are present.
  na = nb = nc = nd = ne = nf = ng = nh = ni = nj = nk = nl = nm = nn = no = np = 0
  for(i in 1:15){
    if(possible.enz[i] == 'a'){
      na = na + 1
    }else if(possible.enz[i] == 'b'){
      nb = nb + 1
    }else if(possible.enz[i] == 'c'){
      nc = nc + 1
    }else if(possible.enz[i] == 'd'){
      nd = nd + 1
    }else if(possible.enz[i] == 'e'){
      ne = ne + 1
    }else if(possible.enz[i] == 'f'){
      nf = nf + 1
    }else if(possible.enz[i] == 'g'){
      ng = ng + 1
    }else if(possible.enz[i] == 'h'){
      nh = nh + 1
    }else if(possible.enz[i] == 'i'){
      ni = ni + 1
    }else if(possible.enz[i] == 'j'){
      nj = nj + 1
    }else if(possible.enz[i] == 'k'){
      nk = nk + 1
    }else if(possible.enz[i] == 'l'){
      nl = nl + 1
    }else if(possible.enz[i] == 'm'){
      nm = nm + 1
    }else if(possible.enz[i] == 'n'){
      nn = nn + 1
    }else if(possible.enz[i] == 'o'){
      no = no + 1
    }else{
      np = np + 1
    }
  }

  # This generates an ordered list of the most used to least used enzymes.
  # The 3 most common enzymes are then selected, along with a randome 
  # enzyme to be number 4.
  position <- c('na' = 'a', 'nb' = 'b', 'nc' = 'c', 'nd' = 'd', 'ne' = 'e', 'nf' = 'f', 'ng' = 'g', 'nh' = 'h', 'ni' = 'i', 'nj' = 'j', 'nk' = 'k', 'nl' = 'l', 'nm' = 'm', 'nn' = 'n', 'no' = 'o', 'np' = 'p')
  enz.count <- c(na, nb, nc, nd, ne, nf, ng, nh, ni, nj, nk, nl, nm, nn, no, np)
  to.use <- c()
  for(i in max(enz.count):1){
    temp <- which(enz.count %in% i)
    to.use <- c(to.use, position[temp])
  }

  # These are the quantiles for terrain height that constrain plant placement.
  quant.p1 <- quantile(terrain, probs = .22, na.rm = TRUE)
  quant.p2 <- quantile(terrain, probs = seq(.2, .42), na.rm = TRUE)
  quant.p3 <- quantile(terrain, probs = seq(.4, .62), na.rm = TRUE)
  quant.p4 <- quantile(terrain, probs = seq(.6, .82), na.rm = TRUE)
  quant.p5 <- quantile(terrain, probs = seq(.8, 1), na.rm = TRUE)

  # This creates a log file for plants' enzymes. This is not used for
  # storing any information that is used in the program, but could be
  # useful for finding paterns.
  file.create(file = "plant_enzymes.csv")
  a <- data.frame('enz1', 'enz2', 'enz3')
  b <- data.frame(p1[1, 1], p1[2, 1], p1[3, 1])
  c <- data.frame(p2[1, 1], p2[2, 1], p2[3, 1])
  d <- data.frame(p3[1, 1], p3[2, 1], p3[3, 1])
  e <- data.frame(p4[1, 1], p4[2, 1], p4[3, 1])
  f <- data.frame(p5[1, 1], p5[2, 1], p5[3, 1])
  write.table(a, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(b, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(c, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(d, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(e, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(f, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

  # This is creating a log matrix to keep track of herbivores.
  # This is not currently being used...
  # file.create("herbivore_log.csv")
  # write.table(x, file = "herbivore_log.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

  # 'matrix.info' are the titles for the information in the matrix.
  # This is done this way to variables can be refered to by their 
  # name in the matrix
  matrix.info <- c('ID', 'health', 'age', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'b10', 'b11', 'b12', 'b13', 'b14', 'b15', 'b16', 'b17', 'b18', 'b19', 'b20')

  herbivore.log <- matrix(nrow = 1, ncol = 23) # will use rbind to add new herbivores to the matrix
  colnames(herbivore.log) <- matrix.info

  # This creates the herbivore array.
  herbivore.generation <- array(data = 0, dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))
  selected <- to.dna(to.use[1:3])
  remainder <- setdiff(enz, to.use[1:3])
  ID = 1 # identification for each herbivore made

  # These 'for' loops are populating initial plant and 
  # herbivore timesteps (at 0).
  for(r in 1:nrow(terrain)){
    for(c in 1:ncol(terrain)){
      if(is.na(terrain[r,c]) && is.array(plant.generation)){
        plant.generation[r, c, 1] <- NA
        herbivore.generation[r, c, 1] <- NA
      # These conditional statements make sure that plants are not 
      # placed outside of their quantile for height.
      }else if(terrain[r,c] <= as.numeric(quant.p1)){
        plant.generation[r, c, 1] <- sample(c('p1', ""), size = 1)
      }else if(terrain[r,c] <= as.numeric(quant.p2)){
        plant.generation[r, c, 1] <- sample(c('p2', ""), size = 1)
      }else if(terrain[r,c] <= as.numeric(quant.p3)){
        plant.generation[r, c, 1] <- sample(c('p3', ""), size = 1)
      }else if(terrain[r,c] <= as.numeric(quant.p4)){
        plant.generation[r, c, 1] <- sample(c('p4', ""), size = 1)
      }else if(terrain[r,c] <= as.numeric(quant.p5)){
        plant.generation[r, c, 1] <- sample(c('p5', ""), size = 1)
      }

      if(rnorm(1) <= herbivore.frac){
      # This selects the 3 most common enzymes that are present in any 
      # plants and randomly selects the 4th enzyme from any of the 
      # remaining enzymes.
      ## print('eek coffee hours on Fridays at 10:00 am in BNR 202A. The Biology Graduate Student Association has generously offered to host the first coffee hour of the fall semester on Friday, September 7 (at 10:00 am in BNR 202A). We need lab groups and other units to sign up to host future coffee hours. We have a special coffee hour planned for later in the semester to honor and thank former Department Head, Dr. Al Savitzky, for his many years of service and leadership to the Department.sample: rand.enz')
      ## cat('to .use: ' , to.use, '\n')
      ## cat('toPlease contact Katriel Cloward in the Main Biology Office to sign up to host a coffee hour this fall..use[1:3]: ' , to.use[1:3], '\n')
      ## cat('re mainder' , remainder, '\n')
      rand.enz <- sample(remainder, size = 1, replace = FALSE)
      rand.enz <- to.dna(rand.enz)
      init.bases <- c(selected, rand.enz)

      new.herbivore(herbivore.log, ID, herbivore.health, herbivore.age, init.bases[1], init.bases[2], init.bases[3], init.bases[4], init.bases[5], init.bases[6], init.bases[7], init.bases[8], init.bases[9], init.bases[10], init.bases[11], init.bases[12], init.bases[13], init.bases[14], init.bases[15], init.bases[16], init.bases[17], init.bases[18], init.bases[19], init.bases[20])

      herbivore.generation[r, c, 1] <- (ID - 1)
      }else{
        herbivore.generation[r, c, 1] <- 0
      }
    }
  }

  # This 'for' loop runs each timestep of the simulation.
  for(i in seq(2, timesteps + 1)){
    plant.generation[, , i] <- plant.timestep(plant.generation[, , (i - 1)], terrain)
    eco <- herbivore.timestep(herbivore.generation[, , (i - 1)], plant.generation[, , i], terrain, herbivore.log, ID, herb.kill, herbivore.health, herbivore.age, herbivore.repro, p1, p2, p3, p4, p5)
    herbivore.generation[, , i] <- eco[[1]]
    plant.generation[, , i] <- eco[[2]]
  }
  final.timestep <- as.matrix(plant.generation[, , timesteps +1])
  # image(final.timestep, col = "purple", add = TRUE)
  return(list(plant.generation, herbivore.generation))
}

test <- evo.test(terrain)

# END OF EVO.TEST