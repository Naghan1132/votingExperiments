set.seed(2023)


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
experiments <- function(n_simulations = 10) {
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
      #situation <- simulation(max(n_voters),max(n_candidates)) # on génére le max puis on prends des samples
      # on récréer les samples à chaque nouveau type :
      # Échantillon aléatoire de lignes et de colonnes
      #sample <- c()
      # faire en sorte que les votants/candidats déjà pris en compte le soit encore
      #lignes_echantillon <- sample(, n_lignes)
      #colonnes_echantillon <- sample(ncol(matrice), n_colonnes)
      #situation <- situation[lignes_echantillon,colonnes_echantillon]

      #créer une liste d'échantillons pour chaque cas !!!
      # créer n_voters*n_candidates situation (sample)
      # for(voter in n_voters){
      #   for(candidate in n_candidates){
      #
      #   }
      # }

      # =====

      for(voter in n_voters){
        for(candidate in n_candidates){
          winners <- c()
          # TEST => on tire un nouveau score à chaque fois
          situation <- simulation(voter,candidate)
          for(method in methods_names){
            scrutin <- get(method)
            winner <- scrutin(situation) # OK
            winners <- c(winners,winner) # on ajoute le gagnant de la méthode
          }
          nouvelle_ligne <- c(n, voter, candidate, type, winners)
          df[cpt,] <- nouvelle_ligne # OU df <- rbind(df, nouvelle_ligne)
          cpt <- cpt +1
        }
      }
    }
  }
  View(df)
  return(NULL)
}



#' Export experiments to excel
#' @export
#' @import writexl
#' @param df dataFrame
export_experiments_to_excel <- function(df){
  writexl::write_xlsx(df,"experiments.xlsx")
}
