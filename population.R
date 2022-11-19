make_random_move <- function(pos, m, visited)
{
  available_moves <- c()
  
  if ( (pos['col'] + 1 <= ncol(m)) && (m[ pos['row'] , pos['col'] + 1] != '#') )
    available_moves <- c(available_moves,1)
  
  if ( (pos['row'] - 1 > 0) && (m[ pos['row'] - 1, pos['col'] ] != '#') )
    available_moves <- c(available_moves,2)
  
  if ( (pos['col'] - 1 > 0) && (m[pos['row'], pos['col'] - 1] != '#') )
    available_moves <- c(available_moves,3)
  
  if ( (pos['row'] + 1 <= nrow(m)) && (m[pos['row'] + 1, pos['col']] != '#') )  
    available_moves <- c(available_moves, 4)
  
  available_visited <- c()
  
  for (move in available_moves)
  {
    new_pos <- new_position(pos, move)
    available_visited <- c(available_visited, visited[ new_pos['row'], new_pos['col'] ])
  }
  least_visited <- which( available_visited == min(available_visited) )
  
  lv <- c()
  for (i in least_visited)
    lv <- c(lv, available_moves[i])
  
  if (length(lv) == 1)
    return(lv[1])
  return(sample(lv,1))
  
  #return( sample(available_moves, 1) )
}

new_position <- function(pos, move)
{
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
  return(pos)
}


initPopulationAdvanced <- function(object, maze)
{
  lower <- object@lower
  upper <- object@upper
  nvars <- length(lower)
  start <- which( maze == 'S', arr.ind = TRUE )[1,]
  population <- matrix( as.double(NA), nrow = object@popSize, ncol = nvars )
  
  for (i in 1:object@popSize)
  {
    pos <- start
    m <- maze
    visited <- matrix(as.integer(0), nrow(maze), ncol(maze))
    for (j in 1:nvars)
    {
      move <- make_random_move(pos, m, visited)
      population[i,j] <- move
      visited[ pos['row'], pos['col'] ] <- visited[ pos['row'], pos['col'] ] + 1
      #m[pos['row'], pos['col']] <- '#'
      pos <- new_position(pos, move)
    }
  }
  return(population)
}
