#' plotCad
#'
#' @param zoneEtude Objet sf contenant nos parcelles
#' @param printID Si TRUE, les noms des cadastres sont affichees. Il faut passer la souris sur les points rouges
#' @param printPlacIFN Si TRUE, on telecharge les placettes IFN autour de la zone d'etude
#' @param bufferIFN Distance de prospection de placettes IFN autour de la zoneEtude
#' @param mapBackground Choix du fond de carte : "OpenStreetMap" , "Scan25" , "Ortho"
#'
#' @return Une carte interactive permettant de voir l'emplacement de nos cadastres et les placettes IFN prochent
#' @export
#'
#' @import leaflet dplyr sf
#'
plotCad <- function(zoneEtude, mapBackground = "OpenStreetMap", printID = FALSE, printPlacIFN = TRUE, bufferIFN = 1500){

   if (mapBackground == "Ortho"){
     background = "GeoportailFrance.orthos"
   }else if (mapBackground == "Scan25"){
      background = "GeoportailFrance.ignMaps"
   }else if (mapBackground == "OpenStreetMap"){
      background = "OpenStreetMap.France"
   }else{
      stop("L'argument mapBackground n'est pas rempli. Choisir : \"OpenStreetMap\" , \"Scan25\" ou \"Ortho")
   }

   # Coordonnees WPS84 pour plot avec leaflet
   zoneEtude = st_transform(zoneEtude,4326)

   # Plot des parcelles cadastrales
   res = leaflet(zoneEtude) %>%
      addProviderTiles(background) %>%
      addPolylines(
         opacity = 100,
         stroke = TRUE,
         weight = 1,
         fill = FALSE,
         color = "black")

   # On rajoute les identifiants des parcelles
   if(printID == TRUE & dim(zoneEtude)[2] > 1){

      # On extrait les coordonnees lat et long
      latLong = zoneEtude %>%
         st_centroid() %>%
         st_coordinates()

      Lng = latLong[,1]
      Lat = latLong[,2]

      res = res %>%
         addCircleMarkers(data = zoneEtude, lng = Lng, lat = Lat, label = zoneEtude$id,
                          color = "red", radius = NULL, opacity = 0.5)
   }

   if(printPlacIFN == TRUE){

      placIFN = importPlacettesIFN(zoneEtude,buffer = bufferIFN) %>% st_transform(4326)

      # Securite si il n'y a pas de placettes IFN
      if(dim(placIFN)[1] == 0){
         print("Agrandir le bufferIFN")
      }else{

         IdIFN = placIFN$idp

         res = res %>%
            addMarkers(data = placIFN,
                       label = IdIFN)
      }
   }

   return(res)
   print(res)
}
