#' plotMH
#'
#' @param MHShp Objef sf contenant le shape des monuments historiques de France
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#' @param bufferMH Perimetre de prospection autour de la zoneEtude
#'
#' @return Renvoi une carte interactive representant le zoneEtude ainsi que les monuments historiques dans la zoneEtude
#' @export
#'
#' @import sf dplyr leaflet
#'
plotMH <- function(MHShp, zoneEtude, bufferMH = 50000) {

   # Coordonnees en Lambert94
   MHShp = st_transform(MHShp, 2154)
   zoneEtude = st_transform(zoneEtude, 2154)

   # Distance maximale pour aller chercher les zones de captages
   MHShp = MHShp %>% st_crop(st_buffer(zoneEtude, dist = bufferMH))

   if (dim(MHShp)[1] == 0){
      stop(paste("Il n'existe pas de Monuments Historiques dans un perimetre de",bufferMH,"m autour de la zone d'etude. Essayez d'augmenter la taille du bufferMH."))
   }

   # Coordonnees en WPS84 pour plot avec leaflet
   zoneEtude = st_transform(zoneEtude, 4326)
   MHShp = st_transform(MHShp, 4326)

   print("ATTENTION : Les coordonnees des monuments historiques ne sont pas exactes donc il y a un decalage avec le fond de carte, zoomer pour plus de precision")

   # Plot des donnees
   res = leaflet() %>%
      addTiles() %>%
      addPolylines(
         data = zoneEtude,
         opacity = 1,
         stroke = TRUE,
         weight = 1,
         color = "black"
      ) %>%
      addMarkers(data = MHShp,
                 label = MHShp$tico)


   print(res)
   return(res)
}

