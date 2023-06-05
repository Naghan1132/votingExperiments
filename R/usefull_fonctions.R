create_similarity_matrix <- function(methods_names){
  # Calcule la matrice :
  similarity_matrix <- matrix(0, length(methods_names),length(methods_names))
  colnames(similarity_matrix) <- methods_names
  rownames(similarity_matrix) <- methods_names
  return(similarity_matrix)
}

calculate_similarity <- function(winners,methods_names){
  similarity_matrix <- create_similarity_matrix(methods_names)
  for (i in 1:(length(methods_names) - 1)){
    for (j in (i + 1):(length(methods_names))){
      if (winners[i] == winners[j]) {
        similarity_matrix[i, j] <- 1
        similarity_matrix[j, i] <- 1
      }
    }
  }
  diag(similarity_matrix) <- 1
  return(similarity_matrix)
}





# MDS sur la dernière matrice de similarité




