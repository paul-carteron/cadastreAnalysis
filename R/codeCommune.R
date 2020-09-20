#' codeCommune
#'
#' @param path Adresse où se trouve le fichier d'entrees parcCad.xlsx
#'
#' @return Les codes communes associés aux parcellaires
#' @export
#'
#' @import tidyverse
#'
#' @examples
codeCommune <-function(path){

   idParcelle = read_excel(path) %>%
      as_tibble()

   if(is_tibble(idParcelle) == TRUE){
      codeComm = idParcelle %>%
         select(commune) %>%
         unique() %>%
         pull()
   }

   print(paste("La propriete est dans la commune : ",codeComm))
   return(codeComm)
}
