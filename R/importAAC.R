#' importAAC
#'
#' @description Fonction permettant de telecharger les Aires d'Alimentation de Captage.
#' Une AAC désigne la zone en surface sur laquelle l’eau qui s’infiltre ou ruisselle alimente le captage.
#' L’extension de ces surfaces est généralement plus vaste que celle des périmètres de protection de captage.
#'
#' @return Renvoi une couche sf indiquant l'ensemble des AAC de France
#' @export
#'
#' @importFrom curl curl_download
#' @import sf dplyr
#' @importFrom utils unzip
#'
importAAC <- function() {

   # Pas d'adresse specifique car cadastre de la taille de la france
   adress = "https://transcode.geo.data.gouv.fr/services/5e2a1e6dfa4268bc255309c4/feature-types/sa:AAC?format=SHP&projection=WGS84"
   temp <- tempfile()

   print("Importation des Aires d'Alimentations de Captage en cour .............;")

   curl::curl_download(adress, temp, quiet = FALSE)
   temp2 <- tempfile()
   unzip(zipfile = temp, exdir = temp2)

   # Importation du .shp
   shp  <- st_read(temp2) %>% st_transform(2154)

   return(shp)
}
