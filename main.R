
library( GA )

source( "mazes.R" )
source( "utils.R" )
source( "draw.R" )
source( "population.R" )


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
    return( -1 * max_path_len * max_path_len ) # Always worse fit than a path that reaches the end/not hits a wall
    
  # b) End was not reached - fitness based on distance to end
  } else if (tile != 'E') {
    dist_end <- sum( abs( c(pos$y, pos$x) - end ) )
    dist_st <- sum( abs( c(pos$y, pos$x) - start ) )
    return( -(0.9*dist_end + 0.1*(nrow(maze)+ncol(maze)-dist_st)) )
  
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
    # TODO? Mutate path so that it becomes shorter (e.g. cycle removal)
    return( mutate )
  }
  
  coords <- data.frame( dec$coords )
  pos_ix <- if (dec$wall) nrow(coords)-1 else nrow(coords)
  pos <- coords[pos_ix,] # Last valid position
  
  # Valid moves
  dirs <- get_valid_moves( maze, visited, pos )
  
  # Reached a dead end - trace back the path and choose a random alternative
  if (length(dirs) == 0 || runif(1) < 0.05) {
    alt_ixs <- c()
    alt_moves <- c()
    for (i in (pos_ix-1):1) {
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


# Read maze
maze <- maze_to_matrix( maze7 )
max_path_length <- nrow( maze ) * ncol( maze )
draw_maze( maze )

# Run the genetic algorithm
GA <- ga(type = "real-valued",
         fitness = function(path) fitness(path, maze ),
         crossover = ga_spCrossover,
         population = function(obj) initPopulationAdvanced(obj, maze),
         mutation = function(obj, parent) mutation(obj, parent, maze),
         lower = rep(1, max_path_length),
         upper = rep(5, max_path_length),
         popSize = 100,
         maxiter = 1000,
         elitism = 1,
         pmutation = 0.8,
         pcrossover = 0,
         monitor = function(obj) ga_maze_monitor(obj, maze,  update_rate = 10, sleep = 0.1, draw_all = TRUE, opacity = 0.5))

plot( GA )
path_enc <- GA@solution[1,]
result <- decode_path( maze, path_enc )

# Print result
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

