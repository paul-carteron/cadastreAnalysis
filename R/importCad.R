#' @title importCad
#'
#' @param codeINSEE Code INSEE d'une commune de France
#'
#' @return Renvoi les cadastres de la commune selectionnee
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom R.utils gunzip
#' @importFrom sf st_as_sf st_make_valid st_read st_transform
#' @importFrom stringr str_sub
#' @importFrom magrittr "%>%"
#'
importCad <- function(codeINSEE) {

      codeDep = str_sub(code,1,2)
      url = paste0("https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/",
                   codeDep,"/",
                   codeINSEE,"/cadastre-",
                   codeINSEE,
                   "-parcelles.json.gz")

      temp = tempfile(fileext = ".json.gz")

      download.file(url, destfile = temp, mode = "wb")

      sf = st_read(gunzip(temp)) %>%
         mutate(id = paste(commune,section,numero, sep=".")) %>%
         st_as_sf() %>%
         st_transform(4326) %>%
         st_make_valid()

      unlink(temp)


      print(paste("Le telechargement de la commune",codeINSEE,"est termine"))

   return(sf)
}

