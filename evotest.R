evo.test <- function(timesteps = 50, terrain, herbivore.health = c(100), herbivore.age = c(50), herb.frac = c(.1), herb.repro = c(.4), kill = c(.1)){
  info.herb <- setup.herbivores(herbivore.state, eat, kill, herb.repro, herb.frac)

  # create plants array
  plant.generation <- array(data = "", dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))
  # plants are randomly assigned the three enzymes that digest them
  # the order of the enzymes gives the amount of health returned
  Enz <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')
  plants <- list(c('p1'), c('p2'), c('p3'), c('p4'), c('p5'))
  for(i in 1:5){
    temp <- sample(Enz, size = 3, replace = FALSE)
    plants[[i]] <- c(plants[[i]],temp)
  }
  # probability associated with quantile for terrain height
  prob.plant <- c()

  # creating herbivore array
  herbivore.generation <- array(data = 0, dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))
  herbivores <- c(0, 5)
  p.herb <- c((1 - sum(herb.frac)), herb.frac)

  # populating initial plant and herbivore timestep (at 0)
  for(r in 1:nrow(terrain)){
    for(c in 1:ncol(terrain)){
      if(is.na(terrain[r,c])){
        plant.generation[r, c, 1] <- NA
        herbivore.generation[r, c, 1] <- NA
      }else{
        # make sure plants are not placed outside of their quantile for height
        plant.generation[r, c, 1] <- sample(plants, size = 1, replace = TRUE, prob = .5)
        herbivore.generation[r, c, 1] <- sample(herbivores, size = 1, replace = FALSE, prob = p.herb)
      }
    }
  }

  # run each timestep of the simulation
  for(i in seq(2, timesteps + 1)){
    plant.generation[, , i] <- plant.timestep(plant.generation[, , (i - 1)], terrain)
    eco <- herbivore.timestep(herbivore.generation[, , (i - 1)], plant.generation[, , i], terrain, info.herb)
    herbivore.generation[, , i] <- eco[[1]]
    plant.generation[, , i] <- eco[[2]]
  }
  #final.timestep <- as.matrix(plant.generation[, , timesteps +1])
  #image(final.timestep, col = "purple", add = TRUE)
  return(list(plant.generation, herbivore.generation))
}
