#' importDepartement
#'
#' @param folderName Nom du dossier dans lequel telecharger la carte des departements de France
#'
#' @description Telecharge le carte des départements français si ce n'est pas déjà le cas
#'
#' @importFrom here here
#' @importFrom sf st_read write_sf
#'
#' @return Telecharge le carte des departements dans le dossier specifie par foldername
#' @export
#'
importDepartement = function(folderName){

   if (!"carte_departement" %in% list.files(here(folderName))){
      temp <- tempfile(fileext = ".zip")
      temp2 <- tempdir()
      download.file(url = "https://www.data.gouv.fr/fr/datasets/r/eb36371a-761d-44a8-93ec-3d728bec17ce",
                    destfile = temp, mode = "wb")
      unzip(temp, exdir = temp2)

      res <- st_read(temp2)

      dir.create(here(folderName, "carte_departement"))
      write_sf(res, here(folderName, "carte_departement", "carte_departement.shp"))

      cat(paste0("\n \nLa carte des departements du Grand-est a ete telecharge ici : \n   ",
                 here(folderName, "carte_departement"), "\n \n"))
   }else{
      cat(paste0("\n \n La carte des departements du Grand-est est deja telecharge ici : \n   ",
                 here(folderName, "carte_departement"), "\n \n"))
   }

}

