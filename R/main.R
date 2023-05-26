# === Clustering ===
# ACM
# CAH
# MDS

#créer un tableau fixe, faire varier les candidats (nb impairs de pref) etc...

#' experiments function
#' @export
#' @import voteSim
#' @import votingMethods
#' @param n_simulations number of simulations
#' @returns matrix
experiments <- function(n_simulations = 2) {
  #namesM <- c("Uninominal 1 round","Uninominal 2 round","Succesif elimination","Bucklin","Borda","Nanson","Minimax","Copeland","Range Voting","Majority Jugement","Approval")
  methods_names <- c("successif_elimination","bucklin","borda","nanson","minimax","copeland","range_voting","approval")
  simu_types <- c("generate_beta","generate_unif_continu","generate_norm") # generate_beta,generate_unif_continu,generate_norm
  #n_candidates <- c(3,4,5,7,9,14) # OK
  #n_voters <- c(9,15,21,51,101,1001,10001) # OK
  n_candidates <- c(3,5) #test
  n_voters <- c(7,9) #test

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
          winners <- c()
          #situation <- simulation(voter,candidate) # Mettre pour LES tests
          for(method in methods_names){
            scrutin <- get(method)
            # faire exception pour uninominale 2T !!!
            winner <- scrutin(situation[echantillon_final_candidate,echantillon_final_voter]) # test
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
  return(data.frame(df))
}



#' Export experiments to excel
#' @export
#' @import writexl
#' @param df dataFrame
export_experiments_to_excel <- function(df){
  writexl::write_xlsx(df,"experiments.xlsx")
}
