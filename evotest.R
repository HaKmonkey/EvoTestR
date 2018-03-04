evo.test <- function(timesteps = 50, terrain, herbivore.health = c(100), herbivore.age = c(50), herb.frac = c(.1), herb.repro = c(.4), kill = c(.1)){
  info.herb <- setup.herbivores(herbivore.state, eat, kill, herb.repro, herb.frac)

  # create plants array
  plant.generation <- array(data = "", dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))

  # possibly move this to plant setup function (again)
  # look at using reference classes for plants' enzymes
  # plants are randomly assigned the three enzymes that digest them
  # the order of the enzymes gives the amount of health returned
  Enz <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')
  plants <- list(c('p1'), c('p2'), c('p3'), c('p4'), c('p5'))
  for(i in 1:5){
    temp <- sample(Enz, size = 3, replace = FALSE)
    plants[[i]] <- plants[[i]] = temp
  }

  # quantiles for terrain height that constrain plant placement
  prob.p1 <- quantile(terrain, probs = .22)
  prob.p2 <- quantile(terrain, probs = seq(.2, .42))
  prob.p3 <- quantile(terrain, probs = seq(.4, .62))
  prob.p4 <- quantile(terrain, probs = seq(.6, .82))
  prob.p5 <- quantile(terrain, probs = seq(.8, 1))
  p1 <- c("", plants[[1]])
  p2 <- c("", plants[[2]])
  p3 <- c("", plants[[3]])
  p4 <- c("", plants[[4]])
  p5 <- c("", plants[[5]])

  # creating herbivore array
  herbivore.generation <- array(data = 0, dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))
  herbivores <- c(0, herbivore.health)
  p.herb <- c((1 - sum(herb.frac)), herb.frac)

  # populating initial plant and herbivore timestep (at 0)
  for(r in 1:nrow(terrain)){
    for(c in 1:ncol(terrain)){
      if(is.na(terrain[r,c])){
        plant.generation[r, c, 1] <- NA
        herbivore.generation[r, c, 1] <- NA
      }else{
        # make sure plants are not placed outside of their quantile for height
        # setting probabilites now
        if(terrain[r,c] <= prob.p1){
          plant.generation[r, c, 1] <- sample(p1, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= prob.p2){
          plant.generation[r, c, 1] <- sample(p2, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= prob.p3){
          plant.generation[r, c, 1] <- sample(p3, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= prob.p4){
          plant.generation[r, c, 1] <- sample(p4, size = 1, replace = FALSE, prob = .5)
        }else if(terrain[r,c] <= prob.p5)
          plant.generation[r, c, 1] <- sample(p5, size = 1, replace = FALSE, prob = .5)
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
