#' importCad
#'
#' @param codeCommune codes communes étudiés. Si il existe un fichier "parcCad.xlsx", les cadastres sont directement triés
#'
#' @return Renvoi les cadastres souhaités
#' @export
#'
#' @import dplyr sf
#' @importFrom R.utils gunzip
#' @importFrom utils download.file
#' @importFrom stringr str_sub
#'
importCad <- function(codeCommune) {

   res = list()

   for(i in 1:length(codeCommune)){

      code = codeCommune[i]
      codeDep = str_sub(code,1,2)
      url = paste0("https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/",codeDep,"/",code,"/cadastre-",code,"-parcelles.json.gz")

      temp = tempfile(fileext = ".json.gz")

      download.file(url, destfile = temp, mode = "wb")

      sf = st_read(gunzip(temp)) %>%
         mutate(id = paste(commune,section,numero, sep=".")) %>%
         st_as_sf() %>%
         st_transform(4326) %>%
         st_make_valid()

      unlink(temp)

      res[[i]] = sf

      print(paste("Le telechargement de la commune",codeCommune[i],"est termine"))

   }

   return(bind_rows(res))
}
