
library( GA )

# Read mazes
source( "mazes.r" )


# Converts a maze to a 2d matrix
maze_to_matrix <- function( maze )
{
  nrow <- length( maze )
  ncol <- nchar( maze[1] )
  maze_flat <- unlist( strsplit( maze1, '' ) )
  mat <- matrix( maze_flat, nrow, ncol, byrow=TRUE )
  return( mat )
}
