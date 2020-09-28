#' plotMNT
#'
#' @param MNT Raster MNT obtenue avec la fonction import MNT
#' @param zoneEtude Objet sf comportant le/les zones etudiees
#'
#' @import sf leaflet
#' @importFrom grDevices terrain.colors
#' @importFrom raster values
#'
#' @return Imprime la carte avec le MNT
#' @export
#'
plotMNT <- function(MNT, zoneEtude){

   palette = terrain.colors(length(unique(values(MNT))))
   pal = colorNumeric(palette, domain = values(MNT), na.color = NA)

   print("Creation des courbes de niveau ..........")
   courbeNiv = courbeNiveau(MNT)

   print("Creation du graphique en cour ..........")

   res = leaflet() %>%
      addTiles() %>%
      addRasterImage(MNT,colors = pal, opacity = 0.6) %>%
      addLegend(
         pal = pal,
         values = values(MNT),
         title = "Altitude [m]") %>%
      addPolylines(data = zoneEtude,
                   opacity = 1,
                   stroke = TRUE,
                   weight = 1,
                   fill = FALSE,
                   color = "black") %>%
      addPolylines(data = courbeNiv,
                      opacity = 1,
                      weight = 0.6,
                      fill = FALSE,
                      color = "grey")

   print(res)
   return(res)
}
