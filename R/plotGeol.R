#' plotGeol
#'
#' @param geolShp Objet sf contenant les couches geologiques : sortie de la fonction importGeol
#' @param zoneEtude Objet sf contenant la/les zoneEtude
#' @param bufferGeol Buffer autour de la zoneEtude que l'on veut etudier
#' @param mapBackground Choix du fond de carte : "OpenStreetMap" , "Scan25" , "Ortho"

#' @return Renvoi une carte interactive avec les differentes couches geologique
#' @export
#'
#' @import leaflet sf shiny
#'
plotGeol <- function(geolShp, zoneEtude, mapBackground = "OpenStreetMap", bufferGeol = 100) {

   if (mapBackground == "Ortho"){
      background = "GeoportailFrance.orthos"
   }else if (mapBackground == "Scan25"){
      background = "GeoportailFrance.ignMaps"
   }else if (mapBackground == "OpenStreetMap"){
      background = "OpenStreetMap.France"
   }else{
      stop("L'argument mapBackground n'est pas rempli. Choisir : \"OpenStreetMap\" , \"Scan25\" ou \"Ortho")
   }

   geolShp = st_transform(geolShp,2154)
   zoneEtude = st_transform(zoneEtude,2154)

   # Distance maximale pour recuperer la geol
   geolShp = geolShp %>% st_crop(st_buffer(zoneEtude,dist = bufferGeol))

   # Palette de couleur lisible par leaflet
   geolShp$NOTATION  = as.factor(geolShp$NOTATION)
   factpal <- colorFactor("magma" , levels = levels(geolShp$NOTATION), na.color = "black")

   # Coordonnees en WPS94 pour pouvoir plot avec leaflet
   geolShp = st_transform(geolShp,4326)
   zoneEtude = st_transform(zoneEtude,4326)

   shinyApp(
      ui = fluidPage(
         # Application title
         titlePanel("Nom de la couche geologique"),
         # Top panel with county name
         verticalLayout(
            wellPanel(textOutput("id")),
            # the map itself
            mainPanel(
               leafletOutput("map")
            )
         )
      ),
      server = function(input, output) {

         output$map <- renderLeaflet({
            leaflet() %>%
               addProviderTiles(background) %>%
               addPolygons(data = geolShp, stroke = FALSE, fillOpacity = 0.6,
                           color = ~factpal(NOTATION),
                           layerId = ~MI_PRINX) %>%
               addPolylines(data = zoneEtude,
                            opacity = 0.5,
                            stroke = TRUE,
                            weight = 1,
                            fill = FALSE,
                            color = "black")
         })

         # Moments
         observe({
            event <- input$map_shape_click
            output$id <- renderText(geolShp$DESCR[geolShp$MI_PRINX == event$id])
         })

      }
   )

}

