smooth_matrix <- function(matrix, filter) {
  padded_matrix <- matrix(0, nrow = nrow(matrix) + 4, ncol = ncol(matrix) + 4)
  padded_matrix[3:(nrow(matrix) + 2), 3:(ncol(matrix) + 2)] <- matrix
  
  smoothed_matrix <- matrix(0, nrow = nrow(matrix), ncol = ncol(matrix))
  
  for (i in 3:(nrow(matrix) + 2)) {
    for (j in 3:(ncol(matrix) + 2)) {
      window <- padded_matrix[(i - 2):(i + 2), (j - 2):(j + 2)]
      smoothed_matrix[i - 2, j - 2] <- sum(window * filter)
    }
  }
  return(smoothed_matrix)
}

forest_size <- 50
initial_trees <- 100
growth_rate <- 0.1
sunlight <- 0.1
water <- 0.1
colors <- colorRampPalette(c("green", "darkgreen"))(100)#colour palette
soil_colours <- colorRampPalette(c("brown", "black"))(100)#colour palette

#initialize forest grid
forest <- matrix(0, nrow = forest_size, ncol = forest_size)

#smoothed soil layer
soil <- matrix(runif(forest_size*forest_size), nrow = forest_size, ncol = forest_size)
filter <- matrix(1/25, nrow = 5, ncol = 5)
soil <- smooth_matrix(soil, filter)
image(soil, col = soil_colours, axes = FALSE, main = "Soil map")
axis(1, at = seq(0, 1, length.out = forest_size), labels = 1:forest_size)
axis(2, at = seq(0, 1, length.out = forest_size), labels = 1:forest_size)

#plant initial trees randomly
for (i in 1:initial_trees) {
  x <- sample(1:forest_size, 1)
  y <- sample(1:forest_size, 1)
  forest[x, y] <- 1
}

#function to grow trees
grow_trees <- function(forest) {
  for (i in 2:(forest_size - 1)) {
    for (j in 2:(forest_size - 1)) {
      if (forest[i, j] > 0) {
        forest[i, j] <- forest[i, j] + growth_rate * sunlight * water
      }
    }
  }
  return(forest)
}

#function to spread trees
spread_trees <- function(forest, soil) {
  new_forest <- forest # Create a copy of the forest to store new trees
  for (i in 2:(forest_size - 1)) {
    for (j in 2:(forest_size - 1)) {
      if (forest[i, j] > 1) {
        #define neighbours (3 by 3 search model)
        neighbors_i <- c(i - 1, i + 1, i, i, i - 1, i - 1, i + 1, i + 1)
        neighbors_j <- c(j, j, j - 1, j + 1, j - 1, j + 1, j - 1, j + 1)
        #check to see if cell is empty, and within neighbourhood
        valid_neighbors <- which(forest[neighbors_i, neighbors_j] == 0 & 
                                   neighbors_i >= 1 & neighbors_i <= forest_size & 
                                   neighbors_j >= 1 & neighbors_j <= forest_size)
        
        if (length(valid_neighbors) > 0) {
          selected_neighbor <- sample(valid_neighbors, 1)
          new_i <- neighbors_i[selected_neighbor]
          new_j <- neighbors_j[selected_neighbor]
          survival_chance <- soil[new_i, new_j]
          if (!is.na(survival_chance) && runif(1) < survival_chance) {
            new_forest[new_i, new_j] <- 1 # Add new tree to the new_forest
          }
        }
      }
    }
  }
  return(new_forest)
}

#simulate over time
for (t in 1:100) {
  forest <- grow_trees(forest)
  forest <- spread_trees(forest, soil)
  
  normalized_forest <- forest / max(forest)
  
  image(normalized_forest, col = colors, axes = FALSE, main = paste("Forest Simulation t=",t))
  axis(1, at = seq(0, 1, length.out = forest_size), labels = 1:forest_size)
  axis(2, at = seq(0, 1, length.out = forest_size), labels = 1:forest_size)
}



