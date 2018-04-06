terrain <- function(n = 6 , water = .2, noise = c(5, 5)){

	make.terrain <- function(n, sd = noise[1]){
		# making the maktrix for the terrain and assigning the 4 corners
		env <- matrix (NA, nrow = 2 ^ n + 1, ncol = 2 ^ n + 1)
		env[1,1] <- rnorm(1, 0, sd)
		env[1,ncol(env)] <- rnorm(1, 0, sd)
		env[ncol(env),1] <- rnorm(1, 0, sd)
		env[ncol(env),ncol(env)] <- rnorm(1, 0, sd)
		return(env)
	}

	diamond.step <- function(env, sd = noise[2]){
		# finding the center and giving it a value
		center <- mean(c(env[1,1], env[1,ncol(env)], env[ncol(env),1], env[ncol(env),ncol(env)]))
		center <- rnorm(1, center, sd)
		env[ceiling(ncol(env) / 2), ceiling(ncol(env) / 2)] <- center
		return(env)
	}

	square.step <- function(env, sd = noise[2]){
		# finding the 4 points that corospond with the sides of each matrix and giving them values
		center <- env[ceiling((ncol(env) / 2)), ceiling((ncol(env) / 2))]
		env[1, ceiling(ncol(env) / 2)] <- rnorm(1, mean(c(env[1,1], env[1,ncol(env)], center)), sd)
		env[ceiling(ncol(env) / 2), ncol(env)] <- rnorm(1, mean(c(env[1,ncol(env)], env[ncol(env),ncol(env)], center)), sd)
		env[ncol(env), ceiling(ncol(env) / 2)] <- rnorm(1, mean(c(env[ncol(env),ncol(env)], env[ncol(env), 1], center)), sd)
		env[ceiling(ncol(env) / 2), 1] <- rnorm(1, mean(c(env[1,1], env[ncol(env),ncol(env)], center)), sd)
		return(env)
	}

	env <- make.terrain(n)

	# loop through the subseted matricies and apply the diamond.step and then the square.step to all
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

	# changeing the selected quantile on the matrix to NA (water)
	env <- replace(env, env <= quantile(env, probs = water) , NA)

	# creating a second matrix that represents the presence of water
	water <- matrix(0, nrow = nrow(env), ncol = ncol(env))
	water <- replace(water, is.na(env), 1)

	# imaging both the terrain and the water
	# image(env, col = terrain.colors(length(env)))
	# image(water, col = c(NA, "blue"), add = TRUE)

	return(env)
}

##### end of terrain

new.plant <- function(enz1, enz2, enz3, val1 = 25, val2 = 12.5, val3 = 6.25){
  output <- list(enz1 = enz1, enz2 = enz2, enz3 = enz3, val1 = val1,  val2 = val2, val3 = val3)
  class(output) <- "plant"
  return(output)
}

##### end of new.plant

new.loc.plant <- function(plants, row, col, terrain){
  possible.location <- as.matrix(expand.grid(row + c(-1, 0, 1), col + c(-1, 0, 1)))

  # get rid of the points outside of the matrix
  out <- numeric()
  for(i in 1:nrow(possible.location)){
    if(possible.location[i, 1] > nrow(plants) || possible.location[i, 2] > nrow(plants) || possible.location[i, 1] == 0 || possible.location[i, 2] == 0)
      out <- c(out, i)
  }

  if(length(out) != 0)
    possible.location <- possible.location[- out, ]

  # get rid of locations that are water ***
  water <- numeric()
  for(i in 1:nrow(possible.location)){
    if(is.na(plants[possible.location[i, 1], possible.location[i, 2]]))
      water <- c(water, i)
  }

  if(length(water) != 0)
    possible.location <- possible.location[- water, ]

  # get rid of values outside of the height limit
  # quantiles for terrain height that constrain plant placement
  quant.p1 <- quantile(terrain, probs = .22)
  quant.p2 <- quantile(terrain, probs = seq(.2, .42))
  quant.p3 <- quantile(terrain, probs = seq(.4, .62))
  quant.p4 <- quantile(terrain, probs = seq(.6, .82))
  quant.p5 <- quantile(terrain, probs = seq(.8, 1))

  height <- numeric()
  for(i in 1:nrow(possible.location)){
    if(plants[row,col] == p1)
      if(terrain[possible.location[i, 1], possible.location[1, 2]] <= quant.p1)
        height <- c(height, i)
    if(plants[row,col] == p2)
      if(terrain[possible.location[i, 1], possible.location[1, 2]] <= quant.p2)
        height <- c(height, i)
    if(plants[row,col] == p3)
      if(terrain[possible.location[i, 1], possible.location[1, 2]] <= quant.p3)
        height <- c(height, i)
    if(plants[row,col] == p4)
      if(terrain[possible.location[i, 1], possible.location[1, 2]] <= quant.p4)
        height <- c(height, i)
    if(plants[row,col] == p5)
      if(terrain[possible.location[i, 1], possible.location[1, 2]] <= quant.p5)
        height <- c(height, i)
  }

  if(length(height) != 0)
    possible.location <- possible.location[- height, ]

  # if there is only the origin left exit reproduce function **
  if(class(possible.location) != "matrix")
    return(possible.location <- c(possible.location, row, col))

  # get rid of reproducing cell ** possible.location[i,1]
  origin <- numeric()
  for(i in 1: nrow(possible.location)){
    if(possible.location[i, 1] == row && possible.location[i, 2] == col)
      origin <- c(origin, i)
  }

  possible.location <- possible.location[- origin, ]

  # randomly select which location to reproduce in
  new.location <- numeric()
  if(class(possible.location) != "matrix"){
    return(new.location <- c(new.location, possible.location[1], possible.location[2]))
  }else{
    random.row <- sample(seq(1,nrow(possible.location)), 1)
    return(new.location <- c(new.location, possible.location[random.row, 1], possible.location[random.row, 2]))
  }
}

##### end of new.loc.plant

plant.timestep <- function(plants, terrain, info.plant){

    survive <- function(cell){
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

    compete <- function(cell_1, cell_2){
      return(cell_2 <- sample(c(cell_1, cell_2), 1, prob = .5)))
    }

    reproduce.plant <- function(row, col, plants, terrain){
      if(runif(1) <= .5){
        new.location <- new.loc.plant(plants, row, col, terrain)
        # checks for competetion
        if(plants[new.location[1], new.location[2]] == ""){
          plants[new.location[1], new.location[2]] <- plants[row, col]
        }else{
          compete(plants[row,col], plants[new.location[1], new.location[2]])
        }
      }
      return(plants)
    }

    # determine if each plant in each cell survives
    for(r in 1:nrow(terrain)){
      for(c in 1:ncol(terrain)){
        plants[r, c] <- survive(plants[r, c])
        if(is.na(plants[r, c]) == FALSE && plants[r, c] != "")
          plants <- reproduce.plant(r, c)
      }
    }

    return(plants)
  }

##### end of plant.timesteps

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

##### end of new.herbivore

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

##### end of new.loc.herb

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

##### end of herbivore.timestep

evo.test <- function(timesteps = 50, terrain, herbivore.health = 100, herbivore.age = 50, herbivore.frac = .1, herbivore.repro = .5, kill = .1){
  #info.herb <- setup.herbivores(herbivore.health, herbivore.age, herbivore.frac, herbivore.repro, kill)

  # create plants array
  plant.generation <- array(data = "", dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))

  # plants are randomly assigned the three enzymes that digest them
  # the order of the enzymes gives the amount of health returned to the herbivore
  enz <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')

  for(i in 1:3){
    temp <- sample(enz, size = 3, replace = FALSE)
  }
  p1 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(enz, size = 3, replace = FALSE)
  }
  p2 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])


  for(i in 1:3){
    temp <- sample(enz, size = 3, replace = FALSE)
  }
  p3 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(enz, size = 3, replace = FALSE)
  }
  p4 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(enz, size = 3, replace = FALSE)
  }
  p5 <- new.plant(temp[1], temp[2], temp[3])
  possible.enz <- c(possible.enz, temp[1], temp[2], temp[3])

  # creates count values for present enzymes
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

  # generates an ordered list of the most used to least used enzymes, will select the first 3 then 1 random one after
  position <- c('na', 'nb', 'nc', 'nd', 'ne', 'nf', 'ng', 'nh', 'ni', 'nj', 'nk', 'nl', 'nm', 'nn', 'no', 'np')
  enz.count <- c(na, nb, nc, nd, ne, nf, ng, nh, ni, nj, nk, nl, nm, nn, no, np)
  to.use <- c()
  for(i in max(enz.count):1){
    temp <- which(enz.count %in% i)
    to.use <- c(to.use, position[temp])
  }

  # quantiles for terrain height that constrain plant placement
  quant.p1 <- quantile(terrain, probs = .22)
  quant.p2 <- quantile(terrain, probs = seq(.2, .42))
  quant.p3 <- quantile(terrain, probs = seq(.4, .62))
  quant.p4 <- quantile(terrain, probs = seq(.6, .82))
  quant.p5 <- quantile(terrain, probs = seq(.8, 1))
  choice.p1 <- c("", p1)
  choice.p2 <- c("", p2)
  choice.p3 <- c("", p3)
  choice.p4 <- c("", p4)
  choice.p5 <- c("", p5)

  # createing a log file for plants' enzymes
  file.create(file = "plant_enzymes.csv")
  a <- data.frame('enz1', 'enz2', 'enz3')
  b <- data.frame(p1[1], p1[2], p1[3])
  c <- data.frame(p2[1], p2[2], p2[3])
  d <- data.frame(p3[1], p3[2], p3[3])
  e <- data.frame(p4[1], p4[2], p4[3])
  f <- data.frame(p5[1], p5[2], p5[3])
  write.table(a, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(b, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(c, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(d, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(e, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(f, file = "plant_enzymes.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

  # creating a log matrix to keep track of herbivores
  ## file.create("herbivore_log.csv")
  ## write.table(x, file = "herbivore_log.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

  matrix.info <- c('ID', 'health', 'age', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'b10', 'b11', 'b12', 'b13', 'b14', 'b15', 'b16', 'b17', 'b18', 'b19', 'b20')
  herbivore.log <- matrix(nrow = 1, ncol = 23) ## will use rbind to add new herbivores to the matrix
  colnames(herbivore.log) <- matrix.info

  # creating herbivore array
  herbivore.generation <- array(data = 0, dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))
  selected <- to.dna(to.use[1:3])
  remainder <- setdiff(enz, to.use[1:3])
  ID = 1 # identification for each herbivore made

  # populating initial plant and herbivore timestep (at 0)
  for(r in 1:nrow(terrain)){
    for(c in 1:ncol(terrain)){
      if(is.na(terrain[r,c])){
        plant.generation[r, c, 1] <- NA
        herbivore.generation[r, c, 1] <- NA
      }else{
        # make sure plants are not placed outside of their quantile for height
        if(terrain[r,c] <= quant.p1){
          plant.generation[r, c, 1] <- sample(choice.p1, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= quant.p2){
          plant.generation[r, c, 1] <- sample(choice.p2, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= quant.p3){
          plant.generation[r, c, 1] <- sample(choice.p3, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= quant.p4){
          plant.generation[r, c, 1] <- sample(choice.p4, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= quant.p5)
          plant.generation[r, c, 1] <- sample(choice.p5, size = 1, replace = FALSE, prob = .5)
        if(rnrom(1) <= herbivore.frac){
          ## select 3 most common enzymes that are present in any plants and randomly select the 4th enzyme from any of the remaining combinations
          rand.enz <- sample(1, remainder)
          rand.enz <- to.dna(rand.enz)
          init.bases <- c(selected, rand.enz)

          herbivore.generation[r, c, 1] <- new.herbivore(herbivore.log, ID, herbivore.health, herbivore.age, init.bases[1], init.bases[2], init.bases[3], init.bases[4], init.bases[5], init.bases[6], init.bases[7], init.bases[8], init.bases[9], init.bases[10], init.bases[11], init.bases[12], init.bases[13], init.bases[14], init.bases[15], init.bases[16], init.bases[17], init.bases[18], init.bases[19], init.bases[20],)
          ID <- ID + 1
        }
        else{
          herbivore.generation[r, c, 1] <- 0
        }
      }
    }
  }

  # run each timestep of the simulation
  for(i in seq(2, timesteps + 1)){
    plant.generation[, , i] <- plant.timestep(plant.generation[, , (i - 1)], terrain)
    eco <- herbivore.timestep(herbivore.generation[, , (i - 1)], plant.generation[, , i], terrain, herbivore.health, herbivore.age, herbivore.frac, herbivore.repro, kill, ID)
    herbivore.generation[, , i] <- eco[[1]]
    plant.generation[, , i] <- eco[[2]]
  }
  #final.timestep <- as.matrix(plant.generation[, , timesteps +1])
  #image(final.timestep, col = "purple", add = TRUE)
  return(list(plant.generation, herbivore.generation))
}

##### end of evo.test

# genetic sequences will be lists of 12 bases 'ATGC'
# seq <- c(c('A','A','A','A','A'), c('T','T','T','T','T'), c('G','G','G','G','G'), c('C','C','C','C','C'))
# x is one of the 4 vectors inside of seq
# the converted value is the 'name' of the enzyme [can make a list of names for the different values from 0 - 15]

# function that converts 5 base sequence into the enzyme (number)
to.enz <- function(b1, b2, b3, b4, b5){
  x <- list(b1, b2, b3, b4, b5)
  DNA <- list('A' = 0, 'T' = 1, 'G' = 2, 'C' = 3)
  Enz <- list('0' = 'a', '1' = 'b', '2' = 'c', '3' = 'd', '4' = 'e', '5' = 'f', '6' = 'g', '7' = 'h', '8' = 'i', '9' = 'j', '10' = 'k', '11' = 'l', '12' = 'm', '13' = 'n', '14' = 'o', '15' = 'p')
  temp <- 0
  for(i in 1:length(x))
    temp <- temp + as.integer(DNA[x[i]])
  return(Enz[as.character(temp)])
}

##### end of to.enz

to.dna <- function(to.use){
  bases <- c()
  DNA <- list('A', 'T', 'G', 'C')
  dna.list <- as.matrix(expand.grid(DNA, DNA, DNA, DNA, DNA))
  for(i in 1:nrow(dna.list)){
    temp <- 0
    if(temp == 3){
      return bases
    }
    if(as.character(to.enz(dna.list[1,])) %in% to.use){
      bases <- c(bases, dna.list[1, ])
      temp <- temp + 1
    }
  }
}

##### end of to.dna
