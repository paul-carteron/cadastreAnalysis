#' PlotAAC
#'
#' @param AACShp Object sf contenant le shp des AAC de France. Ce dernier peut se recuperer avec la fonction importAAC
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#' @param bufferAAC Perimetre de prospection autour de la zoneEtude
#' @param mapBackground Choix du fond de carte : "OpenStreetMap" , "Scan25" , "Ortho"

#' @return Renvoi une carte interactive avec la zone d'etude et les differentes AAC autour de la zoneEtude
#' @export
#'
#' @import sf dplyr leaflet
#'
plotAAC <- function(AACShp, zoneEtude, mapBackground = "OpenStreetMap", bufferAAC = 1000) {

   if (mapBackground == "Ortho"){
      background = "GeoportailFrance.orthos"
   }else if (mapBackground == "Scan25"){
      background = "GeoportailFrance.ignMaps"
   }else if (mapBackground == "OpenStreetMap"){
      background = "OpenStreetMap.France"
   }else{
      stop("L'argument mapBackground n'est pas rempli. Choisir : \"OpenStreetMap\" , \"Scan25\" ou \"Ortho")
   }

   # Coordonnees en Lambert94
   AACShp = st_transform(AACShp, 2154)
   zoneEtude = st_transform(zoneEtude, 2154)

   # Distance maximale pour aller chercher les zones de captages
   AACShp = AACShp %>% st_crop(st_buffer(zoneEtude, dist = bufferAAC))

   if (dim(AACShp)[1] == 0){
      stop(paste("Il n'existe pas de AAC dans un perimetre de",bufferAAC,"m autour de la zone d'etude. Essayez d'augmenter la taille du bufferAAC."))
   }

   # Coordonnees en WPS84 pour plot avec leaflet
   zoneEtude = st_transform(zoneEtude, 4326)
   AACShp = st_transform(AACShp, 4326)

   # Recuperation des centres pour afficher les noms des zones de captages
   # centre = AACShp %>%
   #    st_centroid()

   # Plot des donnees
   res = leaflet() %>%
      addProviderTiles(background) %>%
      addPolygons(
         data = AACShp,
         stroke = TRUE,
         color = "black",
         fillOpacity = 0.5,
         weight = 0.5,
         fillColor = "deepskyblue"
      ) %>%
      addPolylines(
         data = zoneEtude,
         opacity = 1,
         stroke = TRUE,
         weight = 1,
         color = "black"
      )
   # %>%
   #    addCircleMarkers(data = centre,
   #                     label = centre$NomDeAAC_A,
   #                     color = "darkblue", radius = NULL, opacity = 1)

   print(res)
   return(res)
}

