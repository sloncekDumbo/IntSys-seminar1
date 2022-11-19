
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
  max_dist <- ncol( maze ) + nrow( maze )
  
  # Compute fitness score:
  # a) The path goes through walls
  if (tile == '#') {
    return( -1 * max_path_len * max_path_len  ) # Always worse fit than a path that reaches the end/not hits a wall
    
  # b) End was not reached - fitness based on distance to end
  } else if (tile != 'E') {
    dist <- sum( abs( c(pos$y, pos$x) - end ) )
    return( -1 * max_path_len * (max_path_len - path_len) - dist )
  
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
  
  if (dec$found) {
    return( mutate )
  }
  
  coords <- data.frame( dec$coords )
  pos_ix <- if (dec$wall) nrow(coords)-1 else nrow(coords)
  pos <- coords[pos_ix,] # Last valid position
  
  neigh <- data.frame( dir = c(4, 2, 1, 3),
                       x = c(rep(pos$x, 2), pos$x+1, pos$x-1),
                       y = c(pos$y+1, pos$y-1, rep(pos$y, 2)) )
  neigh <- neigh[1 <= neigh$x & neigh$x <= ncol(maze) & 1 <= neigh$y & neigh$y <= nrow(maze),]
  neigh_maze_ixs <- (neigh$x-1) * nrow(maze) + neigh$y
  neigh <- neigh[maze[neigh_maze_ixs] != '#',]
  
  dirs <- c()
  for (ix in 1:nrow( neigh )) {
    if (nrow( merge(neigh[ix,2:3], coords) ) == 0) {
      dirs <- c(dirs, neigh$dir[ix])
    }
  }
  
  if (length(dirs) == 0) {
    # TODO Generate new path / cut off path at some point / mutate at last position with alternative moves / 
    mutate <- runif( nrow(maze)*ncol(maze), 1, 5 )
    
  } else {
    mutate[pos_ix] <- dirs[sample(length(dirs), 1)]
  }
  
  return( mutate )
}

# Read maze
maze <- maze_to_matrix( maze3 )
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
         elitism = 0,
         pmutation = 0.5,
         pcrossover = 0.5,
         monitor = function(obj) ga_maze_monitor(obj, maze,  update_rate = 1, sleep = 1))

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

