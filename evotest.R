evo.test <- function(timesteps = 50, terrain, herbivore.health = 100, herbivore.age = 50, herbivore.frac = .1, herbivore.repro = .5, kill = .1){
  info.herb <- setup.herbivores(herbivore.health, herbivore.age, herbivore.frac, herbivore.repro, kill)

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

  # creating a log file to keep track of herbivores
  ## going to change this into a matrix
  ##file.create("herbivore_log.csv")
  ##x <- data.frame('ID', 'health', 'age', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'b10', 'b11', 'b12', 'b13', 'b14', 'b15', 'b16', 'b17', 'b18', 'b19', 'b20')
  ##write.table(x, file = "herbivore_log.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

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
