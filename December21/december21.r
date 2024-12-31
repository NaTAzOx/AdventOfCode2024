input_file <- "C:/Users/NaTAzOx/OneDrive/Bureau/AdventOfCode2024/December21/input.txt"
codes <- readLines(input_file)

numeric_keypad <- matrix(c(
  "7", "8", "9",
  "4", "5", "6",
  "1", "2", "3",
  NA, "0", "A"
), nrow = 4, byrow = TRUE)

directional_keypad <- matrix(c(
  NA, "^", "A",
  "<", "v", ">"
), nrow = 2, byrow = TRUE)

find_position <- function(key, keypad) {
  which(keypad == key, arr.ind = TRUE)
}

bfs <- function(start_pos, target_pos, keypad) {
  # Directions : Haut, Bas, Gauche, Droite
  directions <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  direction_names <- c("^", "v", "<", ">")
  
  queue <- list(list(pos = start_pos, path = character(0)))
  visited <- matrix(FALSE, nrow = nrow(keypad), ncol = ncol(keypad))
  visited[start_pos[1], start_pos[2]] <- TRUE
  
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    
    # Si on atteint la cible
    if (all(current$pos == target_pos)) {
      # Ajout explicite du `A` pour appuyer sur la touche
      return(c(current$path, "A"))
    }
    
    # Explore les voisins
    for (i in seq_along(directions)) {
      new_pos <- current$pos + directions[[i]]
      
      if (new_pos[1] >= 1 && new_pos[1] <= nrow(keypad) &&
          new_pos[2] >= 1 && new_pos[2] <= ncol(keypad) &&
          !visited[new_pos[1], new_pos[2]] &&
          !is.na(keypad[new_pos[1], new_pos[2]])) {
        
        visited[new_pos[1], new_pos[2]] <- TRUE
        queue <- append(queue, list(list(
          pos = new_pos,
          path = c(current$path, direction_names[i])
        )))
      }
    }
  }
  
  stop("No path found") # Devrait être impossible si `target_pos` est valide
}

find_shortest_sequence <- function(code, start_pos, keypad) {
  sequence <- character(0)
  
  for (key in strsplit(code, "")[[1]]) {
    target_pos <- find_position(key, keypad)
    moves <- bfs(start_pos, target_pos, keypad)
    sequence <- c(sequence, moves)
    start_pos <- target_pos
  }
  
  return(sequence)
}

# Calculate the complexity of each code and sum them up
total_complexity <- 0

for (code in codes) {
  # Étape 1 : Keypad numérique
  sequence_layer4 <- find_shortest_sequence(code, find_position("A", numeric_keypad), numeric_keypad)
  sequence_layer4_collapsed <- paste(sequence_layer4, collapse = "")
  
  # Étape 2 : Clavier directionnel (robot 2)
  sequence_layer3 <- find_shortest_sequence(sequence_layer4_collapsed, find_position("A", directional_keypad), directional_keypad)
  sequence_layer3_collapsed <- paste(sequence_layer3, collapse = "")
  
  # Étape 3 : Clavier directionnel (robot 1)
  sequence_layer2 <- find_shortest_sequence(sequence_layer3_collapsed, find_position("A", directional_keypad), directional_keypad)
  sequence_layer2_collapsed <- paste(sequence_layer2, collapse = "")
  
  # Étape 4 : Clavier directionnel principal
  sequence_layer1 <- find_shortest_sequence(sequence_layer2_collapsed, find_position("A", directional_keypad), directional_keypad)
  
  # Calcul de la complexité
  numeric_part <- as.numeric(gsub("[^0-9]", "", code))
  complexity <- length(sequence_layer1) * numeric_part
  total_complexity <- total_complexity + complexity
  
  # Debug
  cat("Code:", code, "\n",
      "Layer 4:", sequence_layer4_collapsed, "\n",
      "Layer 3:", sequence_layer3_collapsed, "\n",
      "Layer 2:", sequence_layer2_collapsed, "\n",
      "Layer 1:", paste(sequence_layer1, collapse = ""), "\n",
      "Complexity:", complexity, "\n\n")
}

# Print the total complexity
cat("Total Complexity:", total_complexity, "\n")