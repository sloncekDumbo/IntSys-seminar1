
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
  path_len <- length( dec$path )
  max_path_len <- nrow( maze ) * ncol( maze )
  
  # Compute fitness score:
  # a) The path goes through walls
  if (tile == '#') {
    return( -1 * max_path_len * max_path_len  ) # Always worse fit than a path that reaches the end/not hits a wall
    
  # b) End was not reached - fitness based on distance to end
  } else if (tile != 'E') {
    dist <- sum( abs( c(pos$y, pos$x) - end ) )
    return( -1 * max_path_len * dist )
  
  # c) End was reached - fitness based on path length
  } else {
    return( -1 * path_len )
  }
  
}


# Custom mutation function that takes walls and backtracking into account
mutation <- function( GA, parent, maze )
{
  mutate <- parent <- as.integer(GA@population[parent,])
  dec <- decode_path( maze, mutate )
  visited <- dec$visited
  
  if (dec$found) {
    # TODO? Somehow mutate a found path so that it becomes shorter
    return( mutate )
  }
  
  coords <- data.frame( dec$coords )
  pos_ix <- if (dec$wall) nrow(coords)-1 else nrow(coords)
  pos <- coords[pos_ix,] # Last valid position
  
  # Valid moves
  dirs <- get_valid_moves( maze, visited, pos )
  
  if (length(dirs) == 0) {
    # TODO Generate new valid path / cut off path at some point / mutate at last position with alternative moves / 
    mutate <- runif( nrow(maze)*ncol(maze), 1, 5 )
    
  } else {
    mutate[pos_ix] <- dirs[sample(length(dirs), 1)]
  }
  
  return( mutate )
}

# Read maze
maze <- maze_to_matrix( maze6 )
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
         maxiter = 100,
         elitism = 1,
         pmutation = 1,
         pcrossover = 1,
         monitor = function(obj) ga_maze_monitor(obj, maze,  update_rate = 10, sleep = 0.1))

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

