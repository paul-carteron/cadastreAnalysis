#' importMNT
#'
#' @param zoneEtude L'objet sf contenant nos parcelles cadastrales
#' @param buffer Zone de prospection en metre
#' @param zoom Nibveau de precision du MNT. Voir fonction "get_elev_raster" du package elevatr pour plus d'infos.
#'
#' @return Renvoi le MNT de la zoneEtude
#' @export
#'
#' @import sf rgdal
#' @importFrom elevatr get_elev_raster
#'
importMNT <- function(zoneEtude, buffer = 100, zoom = 10) {

   latitude = st_transform(zoneEtude,4326) %>%
      st_coordinates() %>%
      as.data.frame() %>%
      pull(Y) %>%
      mean()

   ground_resolution = (cos(latitude * pi/180) * 2 * pi * 6378137) / (256 * 2^zoom)

   buffer = st_buffer(st_transform(zoneEtude,2154),buffer)

   print(paste("Importation du MNT en cour a la resolution :",round(ground_resolution,2),"m"))

   MNT =  get_elev_raster(buffer, z = zoom, clip = "bbox")

   return(MNT)
}
