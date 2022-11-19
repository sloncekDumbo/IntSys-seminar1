
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
  visited <- matrix( FALSE, nrow(maze), ncol(maze) )
  
  found_path <- FALSE
  hit_wall <- FALSE
  
  for (move in path) {
    
    # Path is only valid until backtracking starts
    if (visited[pos['row'], pos['col']]) {
      res <- res[1:length(res)-1]
      xs <- xs[1:length(xs)-1]
      ys <- ys[1:length(ys)-1]
      break
    }
    visited[pos['row'], pos['col']] <- TRUE
    
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
    
    # Store coordinates for drawing maze
    xs <- c(xs, pos['col'])
    ys <- c(ys, pos['row'])
    
    # Check current position
    in_bounds <- 1 <= pos['col'] && pos['col'] <= ncol(maze) && 1 <= pos['row'] && pos['row'] <= nrow(maze)
    curr <- if (in_bounds) maze[pos['row'], pos['col']] else '#'
    
    if (curr == '#') {
      hit_wall <- TRUE
      break
      
    } else if (curr == 'E') {
      found_path <- TRUE
      break
    }
    
  }
  
  ret <- list(
    "found" = found_path,
    "wall" = hit_wall,
    "path" = res,
    "visited" = visited,
    "coords" = list( "x" = xs, "y" = ys )
  )
  return( ret )
}
