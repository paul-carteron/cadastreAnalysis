#' importMNT
#'
#' @param zoneEtude  objet de class sf corresopndant a un unique polygone
#' @param res resolution du raster : "25m" ou "5m"
#' @param codeDep si non renseigne, le departement est detecte automatiquement (+ 5/10s). Attention, il faut bien verifier que les polygones en entrees soient TOUS dans le codeDep renseigne.
#' @param source si TRUE, la fonction renvoi la source du MNT au format shp
#'
#' @description Permet de telecharger les MNT de l'IGN a la resolution 5m et 25m sur toute la France y compris les DOM-TOMs
#'
#' @usage importMNT(zoneEtude, res = "25m", codeDep, source = FALSE)
#'
#' @importFrom dplyr filter pull
#' @importFrom archive archive archive_extract
#' @importFrom here here
#' @importFrom purrr exec map map_chr reduce
#' @importFrom sf st_crs st_intersection st_read st_transform st_union
#' @importFrom stars read_stars
#' @importFrom stringr str_extract str_sub str_subset
#' @importFrom rlang .data
#'
#' @return renvoi le MNT de la zone etudiee ou la source de la donnee si "source" = TRUE
#' @export

importMNT = function(zoneEtude, res = "25m", codeDep, source = FALSE){

   # ---- Securite sur les arguments : class & oublie ----
   if (missing(zoneEtude)){
      stop("La zone d'etude n'a pas ete renseigne dans la fonction \n\n")
   }

   if (!class(zoneEtude)[1] %in% "sf"){
      stop("La zone d'etude doit etre un objet de class \"sf\" \n\n")
   }

   if (!res %in% c("25m","5m")){
      stop("res doit etre egal a  : \"25m\" ou \"5m\" \n\n")
   }

   zoneEtude = invisible(st_union(zoneEtude))

   # ---- Creation du dossier qui va contenir les MNTs ----
   if (!"MNT data" %in% list.files(here())){
      dir.create(here("MNT data"))
   }

   if (!res %in% list.files(here("MNT data"))){
      dir.create(here("MNT data",res))
   }

   cat(paste0("Tous les fichiers telecharges seront disponible a cette adresse :\n",
              here("MNT data"),
              "\n\n"))

   # ---- Detection automatique du departement si non renseigne ----
   if (missing(codeDep)){
      cat("Detection du departement de la zone etudiee...\n\n")
      codeDep = detectDepartement(zoneEtude)
   }else if (!is.character(codeDep)){
      stop("Le codeDep doit etre au format character (ex : \"29\") \n\n")
   }

   cat(paste0("La zone etudiee se trouve dans le : ",codeDep,"\n\n"))

   # Permet de prendre en compte les numeros de dep a trois chiffres pour le detection des fichiers deja telecharges
   if (nchar(codeDep) == 2){
      codeDepVerif = paste0("0",codeDep)
   }else{
      codeDepVerif = codeDep
   }

   # On detecte les departement deje existants
   alreadyDownload = list.files(here("MNT data",res)) %>%
      str_sub(start = -14, end = -12)

   if (codeDepVerif %in% alreadyDownload){
      cat(paste0("Les MNT du ",codeDep," a ",res," sont deja telecharges\n\n"))
   }else{
      # recuperation du liens de telechargemen
      liens = liens_MNT_IGN %>%
         filter(.data$dep == codeDep, .data$pas == res) %>%
         pull(2)

      # Telechargement et extraction
      tf <- tempfile()
      download.file(liens, tf, mode = "wb" )
      archive_extract(archive(tf), dir = here("MNT data",res))
   }

   # ---- Detection du fichier contenant les dalles et les sources ----
   pattern = c(codeDepVerif, "3_SUPPLEMENTS_LIVRAISON")

   pathToFolder = map(.x = pattern,
                      .f = ~ str_subset(list.dirs(here("MNT data",res)),.x)) %>%
      reduce(intersect)

   pathToFolder = pathToFolder[2]

   # ---- Importation des sources ou des dalles----
   if (source == TRUE){
      shpSource = st_read(here(pathToFolder,"source.shp"), quiet = TRUE)
      zoneEtude = st_transform(zoneEtude, st_crs(shpSource))

      return(st_intersection(shpSource ,zoneEtude))
   }else{
      dalles = st_read(here(pathToFolder,"dalles.shp"), quiet = TRUE)
      sysCoord = st_crs(dalles)
      zoneEtude = st_transform(zoneEtude, sysCoord)

      dalles = dalles %>%
         st_intersection(zoneEtude) %>%
         pull(1) %>%
         str_extract("\\d{4}.\\d{4}")
   }

   # ---- Importations des dalles necessaires ----
   pattern = c(codeDepVerif, "1_DONNEES_LIVRAISON")
   pathTodalles = map(.x = pattern,
                      .f = ~ str_subset(list.dirs(here("MNT data",res)),.x)) %>%
      reduce(intersect)

   dallesToLoad = map_chr(.x = dalles,
                          .f = ~ str_subset(list.files(pathTodalles[2]),.x))

   dallesLoad = map(.x = dallesToLoad,
                    .f = ~ read_stars(here(pathTodalles[2],.x)))

   MNT = exec("st_mosaic", !!!dallesLoad)

   attr(MNT, "dimensions")$x$refsys <- sysCoord
   attr(MNT, "dimensions")$y$refsys <- sysCoord

   codeEPSG = strsplit(sysCoord$wkt,split = ",")
   codeEPSG = codeEPSG[[1]][length(codeEPSG[[1]])]
   codeEPSG = substr(codeEPSG, start = 1, stop = nchar(codeEPSG)-2)

   cat(paste0("Le MNT a ete importe dans le systeme de coordonnees :\n",
              sysCoord$input,
              "\n(code EPSG : ",
              codeEPSG,")\n\n"))

   return(MNT)
}
