#' selectID
#'
#' @param zoneEtude Objet sf contenant les cadastres de la zoneEtude
#' @param mapBackground Choix du fond de carte : "OpenStreetMap" , "Scan25" , "Ortho"

#' @return Renvoi un tableau avec les identifiants des zones choisies
#' @export
#'
#' @import shiny leaflet sf
#'
selectID <- function(zoneEtude, mapBackground = "OpenStreetMap") {

   if (mapBackground == "Ortho"){
      background = "GeoportailFrance.orthos"
   }else if (mapBackground == "Scan25"){
      background = "GeoportailFrance.ignMaps"
   }else if (mapBackground == "OpenStreetMap"){
      background = "OpenStreetMap.France"
   }else{
      stop("L'argument mapBackground n'est pas rempli. Choisir : \"OpenStreetMap\" , \"Scan25\" ou \"Ortho")
   }

   assign("idZone", data.frame(), envir = .GlobalEnv)

   centre = zoneEtude %>%
      st_centroid()

   shinyApp(
      ui = fluidPage(
         # Application title
         titlePanel("Nom du cadastre"),
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
               addPolygons(data = zoneEtude,
                           opacity = 100,
                           stroke = TRUE,
                           weight = 1,
                           fill = TRUE,
                           fillColor = NA,
                           color = "black",
                           layerId = ~id) %>%
               addCircleMarkers(data = centre,
                                label = centre$id,
                                color = "red", radius = NULL, opacity = 0.5)
         })

         # Moments
         observe({
            event <- input$map_shape_click
            output$id <- renderText(zoneEtude$id[zoneEtude$id == event$id])
            idZone <<- rbind(zoneEtude$id[zoneEtude$id == event$id],idZone)
         })

      }
   )
}

