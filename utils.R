
# Converts a maze to a 2d matrix
maze_to_matrix <- function( maze )
{
  nrow <- length( maze )
  ncol <- nchar( maze[1] )
  maze_flat <- unlist( strsplit( maze, '' ) )
  mat <- matrix( maze_flat, nrow, ncol, byrow = TRUE )
  return( mat )
}


# Decodes path encoded with numbers
decode_path <- function( maze, path )
{
  start <- which( maze == 'S', arr.ind = TRUE )[1,]
  xs <- start['col']
  ys <- start['row']
  
  res <- c()
  pos <- start
  path <- as.integer( path )
  
  found_path <- FALSE
  hit_wall <- FALSE
  
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
    
    xs <- c(xs, pos['col'])
    ys <- c(ys, pos['row'])
    
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
  
  ret <- list(
    "found" = found_path,
    "wall" = hit_wall,
    "path" = res,
    "coords" = list( "x" = xs, "y" = ys )
  )
  return( ret )
}
