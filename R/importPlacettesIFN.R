#' importPlacettesIFN
#'
#' @param zoneEtude L'objet sf contenant nos parcelles cadastrales
#' @param buffer Zone de prospection en metre
#'
#' @return Renvoi la position des placettes IFN presentes dans le buffer
#' @export
#'
#' @import sf dplyr
#' @importFrom utils download.file
#' @importFrom utils data
#'
importPlacettesIFN <- function(zoneEtude, buffer = 1500){

   zoneBuffer = zoneEtude %>%
      st_transform(2154) %>%
      st_buffer(dist = buffer)

   placIFN <- DataForet::IFNplacettes %>%
      st_as_sf(coords = c("xl93", "yl93"), crs = 2154, remove = F, agr = "constant") %>%
      st_intersection(zoneBuffer) %>%
      group_by(xl93, yl93) %>%
      slice(1)%>%
      ungroup()

   if(dim(placIFN)[1] == 0){
      print("Il n'y a pas de placettes IFN dans le buffer choisi")
   }

   return(placIFN)
}
