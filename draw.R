
library( grid )

source( "utils.R" )


# Draws a given maze
draw_maze <- function( maze )
{
  h <- nrow( maze )
  w <- ncol( maze )
  
  ar <- w / h
  gl <- grid.layout(2,1,widths=unit(1, "null"), height=unit(c(3, 1/ar), c("lines", "null")), respect = TRUE)
  grid.newpage()
  pushViewport( viewport( layout = gl, width = 0.9, height = 0.9, name = "top" ) )
  pushViewport( viewport( layout.pos.row = 1, layout.pos.col = 1 ) )
  grid.text( "Maze", name = "title", gp = gpar(fontsize = 16) )
  upViewport()
  
  pushViewport( viewport( layout.pos.row = 2, layout.pos.col = 1, name = "map" ) )
  
  xs <- rep( 0:(w-1)/w, each = h )
  ys <- rep( (h-1):0/h, w )
  
  colors <- c()
  for (tile in maze) {
    col <- switch( tile,
      "." = "white",
      "#" = "black",
      "S" = "red",
      "E" = "green",
      "T" = "yellow"
    )
    colors <- c(colors, col)
  }
  
  grid.rect( xs, ys, width = 1/w, height = 1/h, just = c("left", "bottom"), gp = gpar( fill = colors ) )
}


# Draws a path on existing maze image
draw_path <- function( maze, path, opacity = 0.5, allow_backtracking = FALSE )
{
  h <- nrow( maze )
  w <- ncol( maze )

  dec <- decode_path( maze, path, allow_backtracking = allow_backtracking )
  coords <- dec$coords
  
  x <- coords$x
  y <- h + 1 - coords$y
  
  # Remove old path and draw new one
  grid.rect( (x-1)/w, (y-1)/h, 1/w, 1/h, just = c("left", "bottom"), gp = gpar( fill = "steelblue", alpha = opacity ), name = "path" )
}


# Monitor for displaying best path during execution of genetic algorithm
ga_maze_monitor <- function( GA, maze, update_rate = 10, sleep = 0.1, console = TRUE, draw_all = FALSE )
{
  fitness <- na.exclude(GA@fitness)
  best_fitness <- max(fitness)
  best_path <- GA@population[which.max(fitness),]
  
  # Draw new path and update plot title every `update_rate` iterations
  if (GA@iter == 1 || GA@iter %% update_rate == 0) {
    grid.remove( "path", global = TRUE, warn = FALSE )
    if (draw_all) {
      for (i in 1:nrow(GA@population)) {
        draw_path( maze, GA@population[i,], opacity = 0.2 )
      }
    } else {
      draw_path( maze, best_path, opacity = 0.5 )
    }
    
    title <- paste( "Iteration = ", GA@iter, ", Fitness = ", best_fitness, sep = "" )
    grid.edit( "title", label = title)
    Sys.sleep( sleep )
  }
  
  # Also print to console if specified
  if (console) {
    sumryStat <- c(mean(fitness), max(fitness))
    sumryStat <- format(sumryStat, digits = getOption("digits"))
    cat(paste("GA | iter =", GA@iter, "| Mean =", sumryStat[1], "| Best =", sumryStat[2]))
    cat("\n")
    flush.console()
  }
}
