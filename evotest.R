evo.test <- function(timesteps = 50, terrain, herbivore.health = c(100), herbivore.age = c(50), herb.frac = c(.1), herb.repro = c(.4), kill = c(.1)){
  info.herb <- setup.herbivores(herbivore.state, eat, kill, herb.repro, herb.frac)

  # create plants array
  plant.generation <- array(data = "", dim = c(nrow(terrain), ncol(terrain), (timesteps + 1)))

  # plants are randomly assigned the three enzymes that digest them
  # the order of the enzymes gives the amount of health returned to the herbivore
  Enz <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')

  for(i in 1:3){
    temp <- sample(Enz, size = 3, replace = FALSE)
  }
  p1 <- new.plant(temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(Enz, size = 3, replace = FALSE)
  }
  p2 <- new.plant(temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(Enz, size = 3, replace = FALSE)
  }
  p3 <- new.plant(temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(Enz, size = 3, replace = FALSE)
  }
  p4 <- new.plant(temp[1], temp[2], temp[3])

  for(i in 1:3){
    temp <- sample(Enz, size = 3, replace = FALSE)
  }
  p5 <- new.plant(temp[1], temp[2], temp[3])

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

  # createing a log files
  file.create(file = "plant_enzymes.csv", row.names = "")
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


  file.create("herbivore_log.csv")
  x <- data.frame('ID', 'health', 'age', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'b10', 'b11', 'b12', 'b13', 'b14', 'b15', 'b16', 'b17', 'b18', 'b19', 'b20')
  write.table(x, file = "herbivore_log.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

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
