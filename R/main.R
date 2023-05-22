set.seed(2023)

#' test
#' @export
#' @returns winner
test <- function() {
  situation <- voteSim::generate_beta(10,4)
  winner <- votingMethods::borda(situation)
  return(winner)
}


#créer un tableau fixe, comme projet python, faire varier les candidats etc...


