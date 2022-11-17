
library( GA )

# Read mazes
source( "mazes.r" )


# Converts a maze to a 2d matrix
maze_to_matrix <- function( maze )
{
  nrow <- length( maze )
  ncol <- nchar( maze[1] )
  maze_flat <- unlist( strsplit( maze, '' ) )
  mat <- matrix( maze_flat, nrow, ncol, byrow = TRUE )
  return( mat )
}

# Read maze
maze <- maze_to_matrix( maze2 )
start <- which( maze == 'S', arr.ind = TRUE )[1,]
end <- which( maze == 'E', arr.ind = TRUE )[1,]
max_path_length <- nrow( maze ) * ncol( maze )

# Decodes path encoded with numbers
decode_path <- function( path )
{
  res <- c()
  found_path <- FALSE
  hit_wall <- FALSE
  pos <- start
  path <- as.integer( path )
  
  for (move in path) {
    
    # Make a move
    if (move == 1) {    # Right
      res <- c(res, "R")
      pos['col'] <- pos['col'] + 1
    }
    else if (move == 2) # Up
    {
      res <- c(res, "U")
      pos['row'] <- pos['row'] - 1
    }
    else if (move == 3) # Left
    {
      res <- c(res, "L")
      pos['col'] <- pos['col'] - 1
    }
    else if (move == 4) # Down
    {
      res <- c(res, "D")
      pos['row'] <- pos['row'] + 1
    }
    
    # Check current position
    curr <- '#'  # Wall by default - for out of bounds cases
    if (1 <= pos['col'] && pos['col'] <= ncol(maze) && 1 <= pos['row'] && pos['row'] <= nrow(maze)) {
      curr <- maze[pos['row'], pos['col']]
    }
    
    if (curr == '#') {
      hit_wall <- TRUE
      break
    }
    else if (curr == 'E') {
      found_path <- TRUE
      break
    }
    
  }
  
  ret <- list( "found" = found_path, "wall" = hit_wall, "path" = res )
  return( ret )
}

# Fitness function
fitness <- function( path )
{
  path = as.integer( path )
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

# Run the genetic algorithm
GA <- ga(type = "real-valued",
         fitness = fitness,
         lower = rep(1, max_path_length),
         upper = rep(5, max_path_length),
         popSize = 100,
         maxiter = 1000,
         elitism = 1)

plot( GA )
path_enc <- GA@solution[1,]
result <- decode_path( path_enc )

if (result$found) {
  print( "Found path :D" )
  print( result$path )
  
} else if (result$wall) {
  print( "Hit a wall :'( My best attempt is:" )
  print( result$path )
  
} else {
  print( "Path not found :'( My best attempt is:" )
  print( result$path )
}

