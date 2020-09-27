#' selectID
#'
#' @param zoneEtude Objet sf contenant les cadastres de la zoneEtude
#'
#' @return Renvoi un tableau avec les identifiants des zones choisies
#' @export
#'
#' @import shiny leaflet sf
#'
selectID <- function(zoneEtude) {

   assign("idZone", data.frame(), envir = .GlobalEnv)

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
               addTiles() %>%
               addPolygons(data = zoneEtude,
                           opacity = 100,
                           stroke = TRUE,
                           weight = 1,
                           fill = FALSE,
                           color = "black",
                           layerId = ~id)
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

