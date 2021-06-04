#' importMNS
#'
#' @description Permet de télécharger les MNS du Grand-Est.
#'
#' @usage importMNS(zoneEtude, rasterRes = 20, codeEPSG = 4326, convertAsRaster = FALSE)
#'
#' @param zoneEtude objet de class sf
#' @param rasterRes resolution du raster de sortie en metre. La resolution minimale est de 0.2m
#' @param codeEPSG code EPSG de la projection. 4326 = WGS84, 2154 = Lambert-93
#' @param convertAsRaster si TRUE, converti l'objet au format du package raster; le temps de calcul sera plus long. Sinon, l'objet sera au format du package "stars"
#'
#' @details La fonction crée un dossier "MNS data" dans le working directory. Tous les fichiers seront telechargé à cet endroit \cr
#' Remarque : la fonction vérifie toujoours que les fichiers ne sont pas déjà téléchargés car les dalles MNS sont lourdes
#'
#' @importFrom dplyr filter mutate pull tibble
#' @importFrom R.utils cat
#' @importFrom fasterize fasterize raster
#' @importFrom here here
#' @importFrom purrr exec is_empty map
#' @importFrom sf st_as_sf st_bbox st_crs st_intersection st_read st_transform write_sf
#' @importFrom stars read_stars st_as_stars st_dimensions st_warp
#' @importFrom stringr str_replace str_sub str_subset
#' @importFrom tools file_ext
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @return Renvoi un objet de class stars ou raster selon l'option choisi dans "convertAsRaster"
#' @export
#'
importMNS <- function(zoneEtude, rasterRes = 20, codeEPSG = 4326, convertAsRaster = FALSE) {

   zoneEtude <- st_transform(zoneEtude, 4326)

   # ---- Securite sur les arguments : class & oublie ----
   if (missing(zoneEtude)) {
      stop("La zone d'etude n'a pas ete renseigne dans la fonction \n \n")
   }

   if (!class(zoneEtude)[1] %in% "sf") {
      stop("La zone d'etude doit etre un objet de class \"sf\" \n \n")
   }

   # ---- Verification que le departement de la zone d'etude se trouve dans
   # le grand-est ---- Creation de la zone de travail si elle n'existe pas
   if (!"MNS data" %in% list.files(here())) {
      dir.create(here("MNS data"))
   }

   # Telechargement des shapes des departements francais si ce n'est pas deja
   # le fait
   importDepartement <- function() {

      if (!"carte_departement" %in% list.files(here("MNS data"))) {
         temp <- tempfile(fileext = ".zip")
         temp2 <- tempdir()
         download.file(url = "https://www.data.gouv.fr/fr/datasets/r/eb36371a-761d-44a8-93ec-3d728bec17ce",
                       destfile = temp, mode = "wb")
         unzip(temp, exdir = temp2)

         res <- st_read(temp2)

         dir.create(here("MNS data", "carte_departement"))
         write_sf(res, here("MNS data", "carte_departement", "carte_departement.shp"))

         cat(paste0("\n \nLa carte des departements du Grand-est a ete telecharge ici : \n   ",
                    here("MNS data", "carte_departement"), "\n \n"))

      } else {
         cat(paste0("\n \n La carte des departements du Grand-est est deja telecharge ici : \n   ",
                    here("MNS data", "carte_departement"), "\n \n"))
      }

   }
   importDepartement()

   # Importation du shape et verification du departement de la zone d'etude
   codeDep <- st_read(here("MNS data", "carte_departement", "carte_departement.shp"),
                      quiet = TRUE) %>%
      st_intersection(zoneEtude) %>%
      pull(.data$code_insee)

   # Erreur si la zone d'etude n'est pas dans le grand est
   if (!codeDep %in% c("54", "57", "67", "68", "08", "10", "51", "52", "88", "55")) {
      stop(paste0("La zone d'etude est dans le ", codeDep, ". \nElle doit etre dans un departement du Grand-Est : 54, 57 , 67, 68, 08, 10, 51, 52, 88, 55 \n \n "))
   } else {
      cat(paste0("La zone d'etude se trouve dans le : ", codeDep, " \n \n"))
   }

   # ---- Dataframe contenant les URL specifique a chaque cas car le site est
   # mal indexe ----
   dataURL <- tibble(dep = c("54", "57", "67", "68", "08", "10", "51", "52", "88",
                             "55")) %>%
      mutate(parentURL = c("https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M20_TIF_L93_D54_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M20_TIF_L93_D57_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M20_TIF_L93_D67_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M20_TIF_L93_D68_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M25_TIF_L93_D08_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M25_TIF_L93_D10_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M25_TIF_L93_D51_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M25_TIF_L93_D52_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M25_TIF_L93_D88_2018/",
                           "https://odgeo.grandest.fr/ORTHOPUB/ORTHO_MNS_0M25_TIF_L93_D55_2018/")) %>%
      mutate(indexURL = c("catalog/index_MNS_54", "metadata/index_MNS_57", "catalog/index_MNS_67",
                          "catalog/index_MNS_68", "catalog/Dallage_MNE_19FD08", "catalog/dalle",
                          "catalog/dalle", "catalog/Dallage_MNE_19FD52", "catalog/index_MNS_88",
                          "catalog/index_MNS_55")) %>%
      mutate(dallesURL = c(rep("DATA", 9), "data"))

   # ---- Creation du fichier pour telecharger les dalles ----
   folderNameIndex <- paste("Index", codeDep)

   if (!folderNameIndex %in% list.files(here("MNS data"))) {
      dir.create(here("MNS data", folderNameIndex))
   }

   # ---- Telechargement des index ---- Securite pour verifier qu'on a
   # exactement les 5 extensions necessaires a la lecture des index
   if (!setequal(c("dbf", "qpj", "prj", "shx", "shp"), file_ext(list.files(here("MNS data",
                                                                                folderNameIndex))))) {
      file.remove(list.files(here("MNS data", folderNameIndex), full.names = TRUE))

      # Creation de URL
      urlDep <- dataURL %>%
         filter(.data$dep == codeDep) %>%
         mutate(urlDep = paste0(.data$parentURL, .data$indexURL)) %>%
         pull(.data$urlDep)

      # Telechargement des index
      for (i in c(".dbf", ".prj", ".qpj", ".shx", ".shp")) {
         download.file(url = paste0(urlDep, i), destfile = here("MNS data", folderNameIndex,
                                                                paste0(folderNameIndex, i)), mode = "wb")
      }

      cat(paste0("Les Index ont ete telecharge ici : \n   ", here("MNS data",
                                                                  folderNameIndex), "\n \n"))
   } else {
      cat("Les Index sont deja telecharge ici : \n   ", here("MNS data", folderNameIndex),
          "\n \n")
   }
   # ---- Importation du modele numerique de surface ----

   # Recuperation des noms de dalles a telecharger
   folderNameDalle <- paste("MNS", codeDep)

   if (!folderNameDalle %in% list.files(here("MNS data"))) {
      dir.create(here("MNS data", folderNameDalle))
      cat(paste("Le fichier", folderNameDalle, "a ete cree a l'adresse : \n   ",
                here("MNS data", folderNameDalle), "\n \n"))
   } else {
      cat(paste("Le fichier", folderNameDalle, "existe deja a cette adresse : \n   ",
                here("MNS data", folderNameDalle), "\n \n"))
   }

   index <- st_read(here("MNS data", folderNameIndex, paste0(folderNameIndex, ".shp"))) %>%
      st_transform(4326)

   dalles <- index %>%
      st_intersection(zoneEtude) %>%
      pull(1) %>%
      # Magnifique, les index ne sont pas uniformes sur le serveur...
      str_replace(".TIF", "") %>%
      # Alors l?, c'est juste scandaleux... Dans les noms de dalles du 08,
      # il y a des - au lieu de _
      str_replace("-", "_")

   # Preparation de l'URL
   urlData <- dataURL %>%
      filter(.data$dep == codeDep) %>%
      mutate(urlData = paste0(.data$parentURL, .data$dallesURL)) %>%
      pull(.data$urlData)

   # Nom des dalles deja telecharge pour eviter les doublons
   dalleTelechargees <- list.files(here("MNS data", folderNameDalle)) %>%
      # Suppression des extension de fichier
      str_sub(1, nchar(.data) - 4) %>%
      # Suppresion des character correspondant aux index car on travail sur
      # les dalles uniquement
      str_subset(pattern = "Index", negate = TRUE)

   # Si des dalles sont deja telecharge, elle ne seront pas retelecharger
   if (!is_empty(dalleTelechargees)) {
      dallesToLoad <- setdiff(dalles, dalleTelechargees)
   } else {
      dallesToLoad <- dalles
   }

   nbDalles <- length(dallesToLoad)

   for (i in 1:length(dallesToLoad)) {

      cat(paste0("\n \n Dalles ", i, "/", nbDalles, "\n \n"))

      i <- dallesToLoad[i]
      i <- paste0(i, ".zip")
      urlDalle <- paste(urlData, i, sep = "/")

      temp <- tempfile()
      download.file(url = urlDalle, destfile = temp, mode = "wb")
      unzip(zipfile = temp, exdir = here("MNS data", folderNameDalle))
   }
   # ---- Resolution du raster ---- Importation de chaque raster dans une
   # liste
   rasters <- map(.x = dalles,
                  .f = ~ read_stars(here("MNS data", folderNameDalle,
                                                     paste0(.x, ".TIF"))))

   if (!missing(rasterRes)) {

      resample <- function(raster, rasterRes) {
         grid <- st_as_stars(st_bbox(raster), dx = rasterRes)
         res <- st_warp(raster, grid)
         return(res)
      }

      cat(paste0("\n \n Les rasters sont en cours de conversion a la resolution : ",
                 rasterRes, "m \n \n"))

      rasters <- map(.x = rasters, .f = ~resample(.x))

   }

   # ---- Fusion des rasters ---- Utilisation de quasiquotation. exec permet
   # d'utiliser les element d'une liste directement comme variable d'une
   # fonction
   MNS <- exec("st_mosaic", !!!rasters)
   cat("\n \n Fusion des dalles en cours... \n \n")

   # ---- Changement de coordonnees du raster ----
   if (!missing(codeEPSG)) {
      attr(MNS, "dimensions")$x$refsys <- st_crs(codeEPSG)
      attr(MNS, "dimensions")$y$refsys <- st_crs(codeEPSG)
   }

   # ---- Convertir l'objet star en objet raster ----
   if (convertAsRaster == TRUE) {
      x <- st_dimensions(MNS)$x$to
      y <- st_dimensions(MNS)$y$to

      MNS <- st_as_sf(MNS)
      # Le temps de gagner avec 'fasterize' par rapport a 'rasterize' est
      # gargantuesque
      rasterTemp <- fasterize::raster(MNS, nrow = x, ncol = y)
      MNS <- fasterize::fasterize(MNS, rasterTemp, names(MNS)[1])
   }

   return(MNS)
}
