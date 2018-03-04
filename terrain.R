#' Make an elevational grid with different quantiles of water
#'
#' A light wrapper around \code{diamond.step}, \code{square.step},
#'      and \code{make.terrain}
#' @param n Size of grid will be (2^n)+1 (grid default is n = 6;
#'     64 x 64 grid)
#' @param water take a decimal values (percentage) to turn a
#'     certain quantile of the terrain into water (default
#'     water = .2 for 20% quantile)
#' @param noise range of random noise to be added at each step
#'     diamond.step and square.step algorithm; vector of length
#'     two, first element is the first standard deviation (used
#'     to draw the seeds for the corners of the grid) and the
#'     second is the last standard deviation for noise at the
#'     finest spatial scale. Standard deviations are evenly
#'     spread out across all depths of the diamond.square algorithm.
#'     (default c(5, 5))
#' @return a terrain matrix and an image; numeric elements indicate
#'     heights, and NAs indicate cells filled with water
#' @examples
#' env <- terrain(8, .25)
#' @export

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
