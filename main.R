
library( GA )

source( "mazes.R" )
source( "utils.R" )
source( "draw.R" )
source( "population.R" )


##########################
#     Configuration      #
##########################

W_DIST_END <- 0.2
W_DIST_START <- 0.8
P_MUT_RANDOM <- 0.1  # Probability of mutation on random move

DRAW_ALL_PATHS <- FALSE
DRAW_UPDATE_RATE <- 5
DRAW_SLEEP <- 0.1


##########################
#     GA Operations      #
##########################

# Fitness function
fitness <- function( path, maze )
{
  # Decode path and retrieve last position
  dec <- decode_path( maze, path )
  coords <- data.frame( dec$coords )
  pos <- if(dec$wall) coords[nrow(coords)-1,] else coords[nrow(coords),] # Last valid pos
  pos_last <- coords[nrow(coords),] # Last absolute pos
  
  in_bounds <- 1 <= pos_last$x && pos_last$x <= ncol(maze) && 1 <= pos_last$y && pos_last$y <= nrow(maze)
  tile_last <- if (in_bounds) maze[pos_last$y, pos_last$x] else '#'
  tile <- maze[pos$y, pos$x]
  
  end <- which( maze == 'E', arr.ind = TRUE )
  start <- which( maze == 'S', arr.ind = TRUE )
  path_len <- length( dec$path )
  max_path_len <- nrow( maze ) * ncol( maze )
  
  # Compute fitness score:
  # a) The path goes through walls
  if (tile == '#') {
    return( -1 * max_path_len * max_path_len )
    
    # b) End was not reached - fitness based on distance to end and from start
  } else if (tile != 'E') {
    dist_end <- sum( abs( c(pos$y, pos$x) - end ) )
    dist_st <- sum( abs( c(pos$y, pos$x) - start ) )
    return( -(W_DIST_END*dist_end + W_DIST_START*(nrow(maze)+ncol(maze)-dist_st)) )
    
    # c) End was reached - fitness based on path length
  } else {
    return( 1 / path_len )
  }
  
}


# Custom mutation function that takes walls and backtracking into account
# One of 2 mutation modes is selected based on the last valid position on path:
# -> If the path can be extended without hitting walls or backtracking, a random
#    valid move is chosen at the last valid position.
# -> Otherwise (dead end) the path is traced back to the beginning and a random
#    valid alternative move is chosen on at random valid position
mutation <- function( GA, parent, maze )
{
  mutate <- parent <- as.integer(GA@population[parent,])
  dec <- decode_path( maze, mutate )
  visited <- dec$visited
  
  # Path leads to the end
  if (dec$found) {
    return( mutate )
  }
  
  coords <- data.frame( dec$coords )
  pos_ix <- if (dec$wall) nrow(coords)-1 else nrow(coords)
  pos <- coords[pos_ix,] # Last valid position
  
  # Valid moves
  dirs <- get_valid_moves( maze, visited, pos )
  
  # Reached a dead end - trace back the path and choose a random alternative
  if (length(dirs) == 0 || runif(1) < P_MUT_RANDOM) {
    alt_ixs <- c()
    alt_moves <- c()
    for (i in pos_ix:1) {
      dirs <- get_valid_moves( maze, visited, coords[i,] )
      if (length( dirs ) > 0) {
        alt_ixs <- c( alt_ixs, rep( i, length( dirs ) ) )
        alt_moves <- c( alt_moves, dirs )
      }
    }
    
    if (length(alt_ixs) == 0 && length(dirs) == 0) {
      stop( "Maze is unsolvable!" )
      
    } else {
      ix <- sample(length(alt_ixs), 1)
      pos_ix <- alt_ixs[ix]
      mutate[pos_ix] <- alt_moves[ix]
      return( mutate )
    }
    
    # Mutate last move
  } else {
    mutate[pos_ix] <- dirs[sample(length(dirs), 1)]
  }
  
  return( mutate )
}


##########################
#     Solving mazes      #
##########################

solve_maze <- function( maze_raw )
{
  # Read maze
  maze <- maze_to_matrix( maze_raw )
  max_path_length <- nrow( maze ) * ncol( maze )
  draw_maze( maze )
  
  # Run the genetic algorithm
  GA <- ga(type = "real-valued",
           fitness = function(path) fitness(path, maze ),
           crossover = ga_spCrossover,
           #population = function(obj) initPopulationAdvanced(obj, maze),
           mutation = function(obj, parent) mutation(obj, parent, maze),
           lower = rep(1, max_path_length),
           upper = rep(5, max_path_length),
           popSize = 100,
           maxiter = 1000,
           run = 100,
           elitism = 1,
           pmutation = 0.8,
           pcrossover = 0.2,
           monitor = function(obj) ga_maze_monitor(obj, maze,
                                   update_rate = DRAW_UPDATE_RATE,
                                   sleep = DRAW_SLEEP,
                                   draw_all = DRAW_ALL_PATHS)) 
  
  # Print result
  path_enc <- GA@solution[1,]
  result <- decode_path( maze, path_enc )
  if (result$found) {
    print( "Found path :D" )
    print( result$path )
    
  } else if (result$wall) {
    print( "Hit a wall :'( My best attempt is" )
    print( result$path )
    
  } else {
    print( "Path not found :'( My best attempt is" )
    print( result$path )
  }
  
  return( GA )
}

GA <- solve_maze( maze7_T )
plot( GA )


#################################
#     Mazes with treasures      #
#################################

find_path <- function( maze, start, end )
{
  # Setup maze points
  max_path_length <- nrow( maze ) * ncol( maze )
  maze[maze == 'T' | maze == 'E' | maze == 'S'] <- '.'
  maze[start[1], start[2]] <- 'S'
  maze[end[1], end[2]] <- 'E'
  
  # Run GA
  # Run the genetic algorithm
  GA <- ga(type = "real-valued",
           fitness = function(path) fitness(path, maze ),
           crossover = ga_spCrossover,
           population = function(obj) initPopulationAdvanced(obj, maze),
           mutation = function(obj, parent) mutation(obj, parent, maze),
           lower = rep(1, max_path_length),
           upper = rep(5, max_path_length),
           popSize = 100,
           maxiter = 100,
           run = 50,
           elitism = 0,
           pmutation = 0.8,
           pcrossover = 0.2,
           monitor = function(obj) ga_maze_monitor(obj, maze,
                                                   update_rate = 5,
                                                   sleep = DRAW_SLEEP,
                                                   draw_all = FALSE,
                                                   console = TRUE)) 
  res <- decode_path( maze, GA@solution[1,] )
  if (res$found) {
    return( c(length(res$path), GA@solution[1,]) )
  } else {
    return( c(10e10, GA@solution[1,]) )
  }
}


fitness_treas_perm <- function( perm, s2t, t2t, t2e )
{
  dist <- 0
  prev_t <- -1
  ix <- 1
  
  for (t in perm) {
    # No path between treasures at beginning
    if (ix == 1) {
      dist <- dist + s2t[t, 1]
    } else {
      tl <- if (prev_t < t) prev_t else t
      tr <- if (prev_t > t) prev_t else t
      dist <- dist + t2t[t2t[,1] == tl & t2t[,2] == tr, 3]
    }
    
    if (ix == length( perm )) {
      dist <- dist + t2e[t, 1]
    }
    
    ix <- ix + 1
    prev_t <- t
  }
  
  return( -dist )
}


solve_maze_treasures <- function( maze_raw )
{
  # Read maze
  maze <- maze_to_matrix( maze_raw )
  max_path_length <- nrow( maze ) * ncol( maze )
  draw_maze( maze )
  
  # Determine position of start, treasures, end
  start <- which( maze == 'S', arr.ind = TRUE )[1,]
  treasures <- which( maze == 'T', arr.ind = TRUE )
  end <- which( maze == 'E', arr.ind = TRUE )[1,]
  
  # Prepare matrix for storing distances and paths
  n <- nrow( treasures )
  s2t <- matrix( 0, n, 1 + max_path_length )
  t2t <- matrix( 0, (n*(n-1))/2, 3 + max_path_length )
  t2e <- matrix( 0, n, 1 + max_path_length )
  
  # Find shortest paths between start and each treasure
  for (i in 1:n) {
    print( paste( "Finding path between start and treasure", i ) )
    s2t[i,] <- find_path( maze, start, treasures[i,] )
  }
  
  # Find shortest path between treasure pairs
  ix <- 1
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      print( paste( "Finding path between treasure", i, "and treasure", j ) )
      sol <- find_path( maze, treasures[i,], treasures[j,] )
      t2t[ix,] <- c(i, j, sol)
      ix <- ix + 1
    }
  }
  
  # Find shortest paths between each treasure and end
  for (i in 1:n) {
    print( paste( "Finding path between treasure", i, "and end" ) )
    t2e[i,] <- find_path( maze, treasures[i,], end )
  }
  
  # Find best pickup order of treasures with permutation GA
  GA <- ga(type = "permutation",
            fitness = function(perm) fitness_treas_perm(perm, s2t, t2t, t2e),
            crossover = gaperm_cxCrossover,
            lower = 1,
            upper = n,
            popSize = 50,
            maxiter = 500,
            run = 100,
            pmutation = 0.2)
  
  # Construct solution from best found permutation
  perm <- GA@solution[1,]
  path <- c()
  prev_t <- 0
  ix <- 1
  
  for (t in perm) {
    # No path between treasures at beginning
    if (ix == 1) {
      d <- s2t[t, 1]
      p <- s2t[t, 2:(2+d-1)]
      path <- c(path, p)
      
    } else {
      tl <- if (prev_t < t) prev_t else t
      tr <- if (prev_t > t) prev_t else t
      d <- t2t[t2t[,1] == tl & t2t[,2] == tr, 3]
      p <- t2t[t2t[,1] == tl & t2t[,2] == tr, 4:(4+d-1)]
      # Invert path if necessary
      if (prev_t > t) {
        p <- (rev(p) + 2) %% 4
      }
      path <- c(path, p)
    }
    
    if (ix == length( perm )) {
      d <- t2e[t, 1]
      p <- t2e[t, 2:(2+d-1)]
      path <- c(path, p)
    }
    
    ix <- ix + 1
    prev_t <- t
  }
  
  grid.edit( "title", label = "Final path with treasures" )
  grid.remove( "path", global = TRUE, warn = FALSE )
  draw_path( maze, path, allow_backtracking = TRUE )
  
  return( path )
}

maze_raw <- maze5_T
path <- solve_maze_treasures( maze_raw )
decode_path( maze, path, allow_backtracking = TRUE )
