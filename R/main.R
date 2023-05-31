# USE :
#   library(devtools)
#   install_github("Naghan1132/votingExperiments")
#   library(votingExperiments)
#   experiments()

# créer un tableau fixe, faire varier les candidats (nb impairs de pref) etc...

#' experiments function
#' @export
#' @import voteSim
#' @import votingMethods
#' @param n_simulations number of simulations
#' @returns dataframe
experiments <- function(n_simulations = 10) {
  start_time <- Sys.time()
  methods_names <- c("uninominal1T","uninominal2T","successif_elimination","bucklin","borda","nanson","minimax","copeland","condorcet","range_voting","approval","majority_jugement")
  simu_types <- c("generate_beta","generate_unif_continu","generate_norm")
  n_candidates <- c(3,4,5,7,9,14) # OK
  n_voters <- c(9,15,21,51,101,1001,10001) # OK

  # Créer un data frame vide avec des colonnes renommées
  col_names <- c("Simu", "nVoters", "nCandidates","typeSimu")
  col_names <- c(col_names,methods_names)
  df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(df) <- col_names
  cpt <- 1
  for(n in 1:n_simulations){
    # Boucle pour parcourir les fonctions
    for (type in simu_types) {
      simulation <- get(type)
      # génère la situation maximale
      situation <- simulation(max(n_voters),max(n_candidates)) # on génére le max puis on prends des samples

      # =======

      first_n_voter <- n_voters[1]
      first_n_candidate <- n_candidates[1]
      echantillon1_voter <- sample(ncol(situation), first_n_voter, replace = FALSE) # init
      echantillon1_candidate <- sample(nrow(situation),first_n_candidate, replace = FALSE) # init
      for(voter in n_voters){
        colonnes_supplementaires <- sample(setdiff(1:ncol(situation), echantillon1_voter),voter-length(echantillon1_voter), replace = FALSE)
        echantillon_final_voter <- c(echantillon1_voter,colonnes_supplementaires)
        for(candidate in n_candidates){
          lignes_supplementaires <- sample(setdiff(1:nrow(situation), echantillon1_candidate),candidate-length(echantillon1_candidate), replace = FALSE)
          echantillon_final_candidate <- c(echantillon1_candidate, lignes_supplementaires)
          condorcet <- "None"
          winners <- c()

          # =======

          for(method in methods_names){
            if(method == "uninominal1T"){
              scrutin <- get("uninominal")
              winner <- scrutin(situation[echantillon_final_candidate,echantillon_final_voter])
            }else if(method == "uninominal2T"){
              scrutin <- get("uninominal")
              winner <- scrutin(situation[echantillon_final_candidate,echantillon_final_voter],2)
            }else if(method == "copeland"){
              scrutin <- get("copeland")
              winner <- scrutin(situation[echantillon_final_candidate,echantillon_final_voter])
              condorcet <- winner$condorcet
              winner <- winner$copeland
              # ajouter colonne condorcet
            }else if(method == "condorcet"){
              winner <- condorcet
            }
            else{
              scrutin <- get(method)
              winner <- scrutin(situation[echantillon_final_candidate,echantillon_final_voter])
            }
            winners <- c(winners,winner) # on ajoute le gagnant de chaque méthodes
          }
          nouvelle_ligne <- c(n, voter, candidate, type, winners)
          df[cpt,] <- nouvelle_ligne # OU df <- rbind(df, nouvelle_ligne) (rbind enlève les colonnes, pas ouf)
          cpt <- cpt +1
        }
      }
    }
  }
  View(df)
  df <- data.frame(df)
  end_time <- Sys.time() - start_time
  print("execution time : ")
  print(end_time)
  # export_experiments_to_excel(df)
  return(df)
}



#' Export experiments to excel
#' @export
#' @import writexl
#' @param df dataFrame
export_experiments_to_excel <- function(df){
  writexl::write_xlsx(df,"experiments.xlsx")
}


# faire tourner ça sur plusieurs de la fac, et concaténer les résultats dans un seul excel.

# set.seed(2023) dans package voteSim() QUAND ON FAIT TOURNER LA SIMU FINALE

# TODO :
# optimiser majority jugement !

