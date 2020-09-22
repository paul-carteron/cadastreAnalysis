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
importPlacettesIFN<-function(zoneEtude, buffer = 1500){

   contour = zoneEtude %>%
      st_transform(2154) %>%
      st_buffer(dist = buffer)

   placettesIFN <- IFNplacettes %>%
      st_as_sf(coords = c("xl93", "yl93"), crs = 2154, remove = F, agr = "constant") %>%
      st_intersection(zoneEtude) %>%
      group_by(xl93, yl93) %>%
      slice(1)%>%
      ungroup()

   return(placettesIFN)
}
