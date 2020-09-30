#' plotTempMean
#'
#' @param TempMeanRaster Raster des temperatures moyennes obtenues avec la fonction importTempMean
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#' @param mapBackground Choix du fond de carte : "OpenStreetMap" , "Scan25" , "Ortho"

#' @return Renvoi une carte interactive de la temperature moyennes autour de notre zoneEtude.
#' @export
#'
#' @import leaflet dplyr
#' @importFrom raster values
#'
plotTempMean <- function(TempMeanRaster, zoneEtude, mapBackground = "OpenStreetMap"){

   if (mapBackground == "Ortho"){
      background = "GeoportailFrance.orthos"
   }else if (mapBackground == "Scan25"){
      background = "GeoportailFrance.ignMaps"
   }else if (mapBackground == "OpenStreetMap"){
      background = "OpenStreetMap.France"
   }else{
      stop("L'argument mapBackground n'est pas rempli. Choisir : \"OpenStreetMap\" , \"Scan25\" ou \"Ortho")
   }

   palette = terrain.colors(length(unique(values(TempMeanRaster))))
   pal = colorNumeric(palette, domain = values(TempMeanRaster), na.color = "black")

   res = leaflet() %>%
      addProviderTiles(background) %>%
      addRasterImage(TempMeanRaster,colors = pal, opacity = 0.6) %>%
      addLegend(
         pal = pal,
         values = values(TempMeanRaster),
         title = "Temperatures [degre]")%>%
         addPolylines(data = zoneEtude,
                      opacity = 1,
                      stroke = TRUE,
                      weight = 1,
                      fill = FALSE,
                      color = "black")
   print(res)
   return(res)
}
