#' plotTempMean
#'
#' @param TempMeanRaster Raster des temperatures moyennes obtenues avec la fonction importTempMean
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#'
#' @return Renvoi une carte interactive de la temperature moyennes autour de notre zoneEtude.
#' @export
#'
#' @import leaflet dplyr
#' @importFrom raster values
#'
plotTempMean <- function(TempMeanRaster, zoneEtude){

   palette = terrain.colors(length(unique(values(TempMeanRaster))))
   pal = colorNumeric(palette, domain = values(TempMeanRaster), na.color = "black")

   res = leaflet() %>%
      addTiles() %>%
      addRasterImage(TempMeanRaster,colors = pal, opacity = 0.6) %>%
      addLegend(
         pal = pal,
         values = values(TempMeanRaster),
         title = "Temperatures [°C]")%>%
         addPolylines(data = zoneEtude,
                      opacity = 1,
                      stroke = TRUE,
                      weight = 1,
                      fill = FALSE,
                      color = "black")
   print(res)
   return(res)
}
