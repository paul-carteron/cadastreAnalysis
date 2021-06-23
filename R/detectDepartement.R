#' detectDepartement
#'
#' @param zoneEtude Objet sf contenant un seul polygone
#'
#' @description Detecte dans quel departement se trouve le polygon en entree
#'
#' @importFrom sf st_intersection st_transform st_crs
#'
#' @return Renvoi le numero de departement du polygon en entree
#' @export
#'
detectDepartement = function(zoneEtude){

   if (missing(zoneEtude)){
      stop("La zone d'etude n'a pas ete renseigne dans la fonction \n\n")
   }

   if (!class(zoneEtude)[1] %in% c("sf","sfc_MULTIPOLYGON")){
      stop("La zone d'etude doit etre un objet de class \"sf\" \n\n")
   }

   zoneEtude = zoneEtude %>%
      st_transform(st_crs(data_departement))

   num_dep = st_intersection(data_departement, zoneEtude)

   num = paste(num_dep[["code_insee"]])
   nom = paste(num_dep[["nom"]])
   liste = paste("- le",num,":", nom, collapse = "\n")

   if(length(num_dep[["code_insee"]]) > 1){
      stop(paste0("La zone etudiee se trouve sur plusieurs departements differents :\n",
                  liste))
   }else{
      return(num_dep[["code_insee"]])
   }

}
