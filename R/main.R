# USE :
#   library(devtools)
#   install_github("Naghan1132/votingExperiments")
#   library(votingExperiments)
#   experiments()

# créer un tableau fixe, faire varier les candidats (nb impairs de pref) etc...

# FAIRE À CHAQUE FOIS => mettre à jour les packages :
#remotes::install_github("Naghan1132/voteSim")
#remotes::install_github("Naghan1132/votingMethods")


#' experiments function
#' @export
#' @import voteSim
#' @import votingMethods
#' @param n_candidates n_candidates
#' @param n_simulations number of simulations
#' @returns dataframe
experiments <- function(n_candidates, n_simulations = 10) {
  start_time <- Sys.time()
  methods_names <- c("uninominal1T","uninominal2T","successif_elimination","bucklin","borda","nanson","minimax","copeland","condorcet","range_voting","approval","majority_jugement")
  simu_types <- c("generate_beta","generate_unif_continu","generate_norm")
  n_voters <- c(9,15,21,51,101,1001,10001) # OK
  # Créer un data frame vide avec des colonnes renommées
  col_names <- c("Simu", "nVoters", "nCandidates","typeSimu")
  col_names <- c(col_names,methods_names)
  df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(df) <- col_names
  cpt <- 1
  # =====
  for(n in 1:n_simulations){
    # Boucle pour parcourir les fonctions
    for (type in simu_types) {
      simulation <- get(type)
      # génère la situation maximale
      situation <- simulation(max(n_voters),n_candidates) # on génére le max puis on prends des samples
      first_n_voter <- n_voters[1]
      echantillon1_voter <- sample(ncol(situation), first_n_voter, replace = FALSE) # init
      for(voter in n_voters){
        colonnes_supplementaires <- sample(setdiff(1:ncol(situation), echantillon1_voter),voter-length(echantillon1_voter), replace = FALSE)
        echantillon_final_voter <- c(echantillon1_voter,colonnes_supplementaires)
        condorcet <- "None" # peut-être inutile
        winners <- c()
        for(method in methods_names){
          if(method == "uninominal1T"){
            scrutin <- get("uninominal")
            winner <- scrutin(situation[,echantillon_final_voter])
          }else if(method == "uninominal2T"){
            scrutin <- get("uninominal")
            winner <- scrutin(situation[,echantillon_final_voter],2)
          }else if(method == "copeland"){
            scrutin <- get("copeland")
            winner <- scrutin(situation[,echantillon_final_voter])
            condorcet <- winner$condorcet
            winner <- winner$copeland
          }else if(method == "condorcet"){
            winner <- condorcet
          }
          else{
            scrutin <- get(method)
            winner <- scrutin(situation[,echantillon_final_voter])
          }
          winners <- c(winners,winner) # on ajoute le gagnant de chaque méthodes
        }
        nouvelle_ligne <- c(n, voter, n_candidates, type, winners)
        df[cpt,] <- nouvelle_ligne
        cpt <- cpt + 1
      }
    }
  }
  View(df)
  df <- data.frame(df)
  end_time <- Sys.time() - start_time
  print("execution time : ")
  print(end_time)
  export_experiments_to_excel(df,n_candidates)
  return(df)
}



#' dissimilarity function
#' @export
#' @import voteSim
#' @import votingMethods
#' @param test_n_v for experiments
#' @param n_simulations number of simulations
#' @returns similarity_matrix
dissimilarity <- function(test_n_v,n_simulations = 10){
  start_time <- Sys.time()
  n_lines <- 0
  methods_names <- c("uninominal1T","uninominal2T","successif_elimination","bucklin","borda","nanson","minimax","copeland","condorcet","range_voting","approval","JM")
  #simu_types <- c("generate_beta")
  simu_types <- c("generate_unif_continu")
  #simu_types <- c("generate_norm")
  #n_voters <- c(9,15,21,51,101,1001,10001) # OK
  n_voters <- c(test_n_v)
  n_candidates <- c(3,4,5,7,9,14) # OK
  # =====
  dissimilarity_matrix <- create_dissimilarity_matrix(methods_names) # Initialisation
  # =====
  for(n in 1:n_simulations){
    for (type in simu_types) {
      simulation <- get(type)
      for(voter in n_voters){
        for(candidate in n_candidates){
          situation <- simulation(voter,candidate)
          n_lines <- n_lines + 1
          condorcet <- "None"
          winners <- c()
          for(method in methods_names){
            if(method == "uninominal1T"){
              scrutin <- get("uninominal")
              winner <- scrutin(situation)
            }else if(method == "uninominal2T"){
              scrutin <- get("uninominal")
              winner <- scrutin(situation,2)
            }else if(method == "copeland"){
              scrutin <- get("copeland")
              winner <- scrutin(situation)
              condorcet <- winner$condorcet
              winner <- winner$copeland
            }else if(method == "condorcet"){
              winner <- condorcet
            }
            else{
              scrutin <- get(method)
              winner <- scrutin(situation)
            }
            winners <- c(winners,winner)
          }
          # On a finit de calculer le gagnants de chaque méthodes
          # on calcule alors la 'dissimilarité' entre chaque méthodes
          dissimilarity_matrix_one_case <- calculate_dissimilarity(winners,methods_names)
          dissimilarity_matrix <- dissimilarity_matrix + dissimilarity_matrix_one_case
        }
      }
    }
  }
  end_time <- Sys.time() - start_time
  print("execution time : ")
  print(end_time)
  print(n_lines)
  save(dissimilarity_matrix,file = "dissimilarity_matrix.RData")
  View(dissimilarity_matrix)
  return(dissimilarity_matrix)
}



#' Export experiments to excel
#' @export
#' @import writexl
#' @param n_candidates n_candidates
#' @param df dataFrame
export_experiments_to_excel <- function(df,n_candidates){
  name <- paste0("experiments_",n_candidates,"_candidates.xlsx")
  writexl::write_xlsx(df,name)
}




# 1261 lignes => 10 simu => 26 mins
# faire tourner ça sur plusieurs de la fac, et concaténer les résultats dans un seul excel.
# !!!! set.seed(2023) dans package voteSim() QUAND ON FAIT TOURNER LA SIMU FINALE !!!!


# ==== Simulations sur PC fac ==== : (faire plutôt 2100 lignes ? Donc 100 n_simus ?)
# n_candidates = 3, n_simus = 10 -> 210 lignes -> 4.55 mins (4.1 mins sur pc perso ^^)
# n_candidates = 4, n_simus = 10 -> 210 lignes -> 4.86 mins
# n_candidates = 5, n_simus = 10 -> 210 lignes -> 5.30 mins
# n_candidates = 7, n_simus = 10 ->
# n_candidates = 9, n_simus = 10 ->
# n_candidates = 14, n_simus = 10 -> 210 lignes -> 6.89 mins
