#' importETPMean
#'
#' @description Fonction permettant de recuperer les temperatures moyennes a partir des donnees MODIS.
#' La raster a une resolution de 500m.
#'
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#' @param firstDate Premiere prise en compte dans la moyenne
#' @param endDate Derniere date prise en compte dans la moyenne. Egale à la date du jour si non indique
#' @param bufferTemp Zone de prospection autour de la zone d'etude
#'
#' @return Renvoi un raster contenant les temperatures moyennees sur la periode choisie
#' @export
#'
#' @import MODISTools dplyr sf
#' @importFrom raster calc
#'
importETPMean <- function(zoneEtude, firstDate = stop("Choisir une date de depart"), endDate = format(Sys.time(), "%Y-%m-%d"), bufferTemp = 4000){

   if(as.character(firstDate) == FALSE){
      stop("firstdate doit etre au format character")
   }

   zoneEtude = st_transform(zoneEtude,4326)

   # Permet de trouver le centre de la zoneEtude
   print("Calcul du barycentre de la zoneEtude")
   centre = st_centroid(keepOutline(zoneEtude)) %>%
      st_coordinates()

   # Recuperation des donnees du MODIS
   print("Importation des donnees du MODIS")
   Subset <- mt_subset(product = "MOD11A2",
                       band = "LST_Day_1km",
                       lon = centre[1],
                       lat = centre[2],
                       km_lr = bufferTemp/1000,
                       km_ab = bufferTemp/1000,
                       start = firstDate,
                       end = endDate)

   # On supprime des valeurs aberrantes
   SubsetPlot = Subset %>%
      filter(value > 7500 | value < 65535) %>%
      filter(value != 0)

   # Conversion en raster puis en degrees
   raster = mt_to_raster(SubsetPlot)
   raster = raster - 273.15

   # Moyenne des rasters (il y a un raster par dates)
   rasterMean = calc(raster, mean)

   return(rasterMean)
}
