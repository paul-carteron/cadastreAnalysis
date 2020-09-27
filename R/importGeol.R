#' importGeol
#'
#' @param zoneEtude Objet sf contenant la zone d'etude
#'
#' @return Renvoi la couche geologoque de la zone etudie
#' @export
#'
#' @importFrom stringr str_sub
#' @import dplyr
#' @importFrom sf st_read
#' @importFrom utils unzip
#'
importGeol = function(zoneEtude){

   codeDep = zoneEtude %>%
      pull(commune) %>%
      unique() %>%
      str_sub(1,2)

   # Initialisation de la liste des resultast dans le cas où il y a plusieurs departements
   res = list()

   # Initialisation du test "doublon" : utile pour les cas particuliers ci-dessous
   doublon = FALSE

   for (i in 1:length(codeDep)){

      # Gestion des cas particuliers des liens
      for (j in codeDep){

         j = as.integer(j)

         if (j > 0 & j < 10){
            dep = paste0("0",j)
         }else if (j %in% c(59,62)){
            dep = "59_062"
            doublon = TRUE
         }else if (j %in% c(75,77,78,91,92,93,94,95)){
            dep = "75_077_078_091_092_093_094_095"
            doublon = TRUE
         }else if (j %in% c(24,33,47)){
            stop("Il n'existe pas de donnes Geol pour ce departement")
         }else{
            dep = as.character(j)
         }
      }

      # Creation de l'adresse permettant de telecharger la carte geol
      adresse = paste0("http://infoterre.brgm.fr/telechargements/BDCharm50/GEO050K_HARM_0",dep,".zip")
      temp = tempfile()
      download.file(adresse,temp)
      temp2 = tempfile()
      unzip(zipfile = temp, exdir = temp2)

      fich = list.files(temp2, pattern = ".shp$", full.names=TRUE)
      fich = grep("S_FGEOL", fich, value = TRUE)

      # Importation dans R de la shape
      shp = st_read(fich)

      # Le resultat est stocke dans la liste
      res[[i]] = shp
   }

   # bind_rows permet de fusionner les shapes de chaque departements
   res = bind_rows(res)

   # Dans les deux cas particuliers on importe pls fois les donnees donc il faut enlever les doublons
   if(doublon == TRUE){
      res = distinct(res)
   }

   return(res)
}
