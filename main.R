
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

# Read maze
maze <- maze_to_matrix( maze1 )
max_path_length <- nrow( maze ) * ncol( maze )
start <- which( maze == 'S', arr.ind = TRUE )[1,]

# Generates an initial population of subjects that move randomly
init_population <- function( object )
{
  subj <- gareal_Population( object )
  subj <- matrix( as.integer(subj), dim(subj) )
  return(subj)
}

fitness <- function( path )
{
  pos <- start
  nrow <- dim( maze ) [1]
  ncol <- dim( maze ) [2]
  path_map <- matrix(FALSE, nrow, ncol)
  path_map[ pos[['row']],  pos[['col']] ] <- TRUE
  
  for (move in path) {
    
    # TODO:
    #  1) Check if current position is wall
    #  2) Check if moving in circles?
    
    if (move == 1)
    {
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
    
    if (0 <= pos[['col']] && pos[['col']] <= ncol && 0 <= pos[['row']] && pos[['row']] <= nrow)
    {
      path_map[ pos[['row']],  pos[['col']] ] <- TRUE
    }
    else
      return(NULL)
  }
  
  
  
  return(path_map)
  
}


# Path encoding:
#  1 - right
#  2 - up
#  3 - left
#  4 - down
GA <- ga(type = "real-valued", fitness = fitness, lower = rep(1, max_path_length), upper = rep(5, max_path_length), population = init_population, popSize = 10, maxiter=10, pmutation = 0, pcrossover = 0)

