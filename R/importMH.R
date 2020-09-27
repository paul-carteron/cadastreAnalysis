#' importMH
#'
#' @description Fonction permettatn de telecharger le shape tout les monuments historique de France
#'
#' @return Renvoi une shape contenant les monuments historique de France
#' @export
#'
#' @import sf
#' @importFrom utils download.file
#'

importMH <- function() {

   url = "https://data.culture.gouv.fr/explore/dataset/liste-des-immeubles-proteges-au-titre-des-monuments-historiques/download/?format=shp&q=France&timezone=Europe/Berlin&lang=fr"
   temp = tempfile()

   print("Monuments historiques en cour de telechargement ........")

   download.file(url,temp, mode="wb")
   temp2 = tempfile()
   unzip(zipfile = temp, exdir = temp2)
   fich = list.files(temp2,pattern = ".shp$",full.names = TRUE)
   shp = st_read(temp2)
   unlink(temp)

   return(shp)
}
