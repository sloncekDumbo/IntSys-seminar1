
library( GA )

source( "mazes.R" )
source( "utils.R" )
source( "draw.R" )


# Fitness function
fitness <- function( path, maze )
{
  start <- which( maze == 'S', arr.ind = TRUE )[1,]
  end <- which( maze == 'E', arr.ind = TRUE )[1,]
  max_path_length <- nrow( maze ) * ncol( maze )
  
  path <- as.integer( path )
  pos <- start
  curr <- 'S'
  path_len <- 0
  walls_hit <- 0
  
  for (move in path) {
    
    path_len <- path_len + 1
    
    # Make a move
    if (move == 1) {
      pos['col'] <- pos['col'] + 1
    }
    else if (move == 2)
    {
      pos['row'] <- pos['row'] - 1
    }
    else if (move == 3)
    {
      pos['col'] <- pos['col'] - 1
    }
    else if (move == 4)
    {
      pos['row'] <- pos['row'] + 1
    }
    
    # Check current position
    curr <- '#'  # Wall by default - for out of bounds cases
    if (1 <= pos['col'] && pos['col'] <= ncol(maze) && 1 <= pos['row'] && pos['row'] <= nrow(maze))
    {
      curr <- maze[pos['row'], pos['col']]
    }
    
    # Reached the end
    if (curr == 'E') {
      break
    
    # Hit a wall
    } else if (curr == '#') {
      walls_hit <- walls_hit + 1
    }
  }
  
  # Compute fitness score:
  # a) The path goes through walls - fitness based on number of walls hit
  if (walls_hit > 0) {
    return( -1 * max_path_length * (ncol(maze) + nrow(maze)) * walls_hit )
    
  # b) End was not reached - fitness based on distance to end
  } else if (curr != 'E') {
    dist <- sum( abs( pos - end ) )
    return( -1 * max_path_length * dist )
  
  # c) End was reached - fitness based on path length
  } else {
    return( -1 * path_len )
  }
  
}


# Read maze
maze <- maze_to_matrix( maze2 )
max_path_length <- nrow( maze ) * ncol( maze )
draw_maze( maze )

# Run the genetic algorithm
GA <- ga(type = "real-valued",
         fitness = function(path) fitness(path, maze ),
         lower = rep(1, max_path_length),
         upper = rep(5, max_path_length),
         popSize = 100,
         maxiter = 1000,
         elitism = 0,
         pmutation = 1,
         pcrossover = 1,
         monitor = function(obj) ga_maze_monitor(obj, maze))

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

