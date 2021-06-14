#' codeINSEE
#'
#' @description Permet de convertir un code postal en code INSEE
#' @usage codeINSEE(code_postal = "29760", interactive = FALSE)
#'
#' @param code_postal Code postal au format "character"
#' @param interactive Si TRUE, permet de selectionner la commune parmis
#'
#' @return Renvoi le code INSEE au format caractere
#' @export
#'
#' @importFrom tcltk tk_select.list
#' @importFrom utils select.list
#'
codeINSEE = function(code_postal = "29760", interactive = FALSE){

   # ---- Securite pour la saisie ----
   if (!is.character(code_postal)){
      stop("Le code postal doit etre une chaine de caractere (ex: \"29760\")")
   }

   # ---- Selection interactive ----
   if (interactive){
      commune = tcltk::tk_select.list(data_commune$commune)
      code_insee = as.character(data_commune[data_commune$commune == commune,"code_insee"])
      return(code_insee)
   }

   # ---- Est-ce que c'est bien un code postal existant ? ----
   if (code_postal %in% data_commune$code_postal){
      communes = data_commune[data_commune$code_postal == code_postal,"commune"]$commune
      # ---- S'il il n'y a qu'une commune qui correspond au code postal pas de choix a faire ----
      if(length(communes) == 1){
         code_insee = as.character(data_commune[data_commune$code_postal == code_postal,"code_insee"])
         return(code_insee)
      # ---- Choix a faire parmis les communes associees au code postal ----
      }else{
         commune = select.list(communes)
         code_insee = as.character(data_commune[data_commune$commune == commune,"code_insee"])
         return(code_insee)
      }
   }else{
      stop("Le code postal n'existe pas")
   }
}
