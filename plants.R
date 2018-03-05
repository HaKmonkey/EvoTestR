new.plant <- function(enz1, enz2, enz3, val1 = 25, val2 = 12.5, val3 = 6.25){
  output <- list(enz1 = enz1, enz2 = enz2, enz3 = enz3, val1 = val1,  val2 = val2, val3 = val3)
  class(output) <- "plant"
  return(output)
}
# ==========================================================================================================
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

# ===========================================================================================================
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
