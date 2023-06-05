create_dissimilarity_matrix <- function(methods_names){
  # Calcule la matrice :
  dissimilarity_matrix <- matrix(0, length(methods_names),length(methods_names))
  colnames(dissimilarity_matrix) <- methods_names
  rownames(dissimilarity_matrix) <- methods_names
  return(dissimilarity_matrix)
}

calculate_dissimilarity <- function(winners,methods_names){
  dissimilarity_matrix <- create_dissimilarity_matrix(methods_names)
  for (i in 1:(length(methods_names) - 1)){
    for (j in (i + 1):(length(methods_names))){
      if (winners[i] != winners[j]) {
        # si différent alors...
        dissimilarity_matrix[i, j] <- 1
        dissimilarity_matrix[j, i] <- 1
      }
    }
  }
  diag(dissimilarity_matrix) <- 0 # a enlever ?
  return(dissimilarity_matrix)
}





# MDS sur la dernière matrice de similarité




