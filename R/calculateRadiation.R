#' calculateRadiation
#'
#' @param MNT Raster du Modele Numerique de Terrain de la zoneEtude obtenu avec la fonction importMNT
#' @param date Date au format "Years-Month-Day", par exemple "1997-04-24", le plus beau jour de 1997
#' @param zoneEtude Objet sf contenant le shape de la zoneEtude
#'
#' @return Renvoi un raster avec les valeurs de rayonnements
#' @export
#'
#' @import insol sf dplyr
#' @importFrom raster mean raster extent as.matrix res
#'
calculateRadiation = function(MNT, zoneEtude, date = as.Date("2019-06-20")){

   date = as.Date(date)

   Centre <- keepOutline(zoneEtude) %>%
      st_centroid() %>%
      st_transform(4326) %>%
      st_coordinates()

   MNT[is.na(MNT[])] <- 0
   cgr=cgrad(MNT)
   MNTm=raster::as.matrix(MNT)
   dl=res(MNT)[1]

   ## Isolation at 30 min interval over the length of the day
   ## RH and temp would cahnge over the dy, here we use a constant value for simplicity
   height=mean(MNTm) # Altitude de la zone
   visibility = 30 # https://fr.wikipedia.org/wiki/Visibilit%C3%A9
   RH = 80 # Humidite relative de l'air
   tempK = 288 # Temperature de l'air, voir pour calculer la temperature moyenne avec les raster temp mean
   tmz = 0 # Timezone : 0 pour la france "https://www.timeanddate.com/time/map/"
   year = as.numeric(format(date, format = "%Y")) # Annee
   month = as.numeric(format(date, format = "%m")) # Mois de l'annee
   day = as.numeric(format(date, format = "%d")) # Jour du mois
   timeh = 12 # ?
   jd=JDymd(year,month,day,hour=timeh) # Computes Julian Day from a given date "https://fr.wikipedia.org/wiki/Jour_julien"
   Iglobal=array(0,dim=dim(MNTm))
   deltat = 0.5 # Intervalle entre chaque calcul, ici tout les demi-heur (0.5*heure)
   lat = Centre[2]
   lon = Centre[1]
   dayl=daylength(lat,lon,jd,tmz) # Duree du lever au coucher du soleil

   for (srs in seq(dayl[1],dayl[2],deltat)){
      jd = JDymd(year,month,day,hour=srs)
      sv = sunvector(jd,lat,lon,tmz)
      hsh = hillshading(cgr,sv)
      sh = doshade(MNTm,sv,dl)
      zenith = sunpos(sv)[2]
      Idirdif = insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.15)
      ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
      ## values in J/m^2
      Iglobal = Iglobal + (Idirdif[,1] * hsh + Idirdif[,2] )*3600*deltat
   }
   ## rasterize to plot nicely
   Iglobal = raster(Iglobal,crs=projection(MNT))
   extent(Iglobal) = extent(MNT)

   return(Iglobal*1e-6)
}
