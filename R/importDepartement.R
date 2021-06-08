#' importDepartement
#'
#' @description Telecharge le carte des départements français si ce n'est pas déjà le cas
#'
#' @importFrom here here
#' @importFrom sf st_read write_sf
#'
importDepartement = function(){

   {
   if (!"carte_departement" %in% list.files(here("MNS data"))){
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
   }
   else
      cat(paste0("\n \n La carte des departements du Grand-est est deja telecharge ici : \n   ",
                 here("MNS data", "carte_departement"), "\n \n"))
   }

}

