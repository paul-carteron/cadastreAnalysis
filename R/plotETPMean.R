#' plotETPMean
#'
#' @param ETPMeanRaster Raster des ETP moyennes obtenues avec la fonction importETPMean
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#'
#' @return Renvoi une carte interactive de la temperature moyennes autour de notre zoneEtude.
#' @export
#'
#' @import leaflet dplyr
#' @importFrom raster values
#'
plotETPMean <- function(ETPMeanRaster, zoneEtude){

   palette = terrain.colors(length(unique(values(ETPMeanRaster))))
   pal = colorNumeric(palette, domain = values(ETPMeanRaster), na.color = "black")

   res = leaflet() %>%
      addTiles() %>%
      addRasterImage(ETPMeanRaster,colors = pal, opacity = 0.6) %>%
      addLegend(
         pal = pal,
         values = values(ETPMeanRaster),
         title = "ETP [mm]")%>%
      addPolylines(data = zoneEtude,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 1,
                   fill = FALSE,
                   color = "black")
   print(res)
   return(res)
}
