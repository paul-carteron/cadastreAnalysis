#' plotCad
#'
#' @param zoneEtude Objet sf contenant nos parcelles
#' @param printID Si TRUE, les noms des cadastres sont affichees. Il faut passer la souris sur les points rouges
#'
#' @return Une carte interactive permettant de voir l'emplacement de nos cadastres et les placettes IFN prochent
#' @export
#'
#' @import leaflet dplyr sf
#'
plotParcelles <- function(zoneEtude, printID = TRUE){

   # Coordonnees WPS84 pour plot avec leaflet
   placIFN = importPlacettesIFN(zoneEtude) %>% st_transform(4326)
   parcelles = st_transform(zoneEtude,4326)

   # Plot des parcelles cadastrales
   res = leaflet(zoneEtude) %>%
      addTiles() %>%
      addPolylines(
         opacity = 100,
         stroke = TRUE,
         weight = 1,
         fill = FALSE,
         color = "blue")

   # On rajoute les identifiants des parcelles
   if(printID == TRUE){

      # On extrait les coordonn?es lat et long
      latLong = zoneEtude %>%
         st_centroid() %>%
         st_coordinates()

      Lng = latLong[,1]
      Lat = latLong[,2]
      Id = parcelles %>%
         mutate(Id = paste(section,numero,sep=".")) %>%
         pull(Id)

      res = res %>%
         addCircleMarkers(lng = Lng, lat = Lat, label = Id,
                          color = "red", radius = NULL, opacity = 0.5)
   }

   # Securite si il n'y a pas de placettes IFN
   if(dim(placIFN)[1] == 0){
      print("Il n'y a pas de placettes IFN : agrandir le buffer dans la fonction importPlacettesIFN")
   }else{

      IdIFN = placIFN$idp

      res = res %>%
         addMarkers(data = placIFN,
                    label = IdIFN)
   }

   print(res)
}
