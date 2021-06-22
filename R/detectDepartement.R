#' detectDepartement
#'
#' @param shape Objet sf contenant un seul polygone
#'
#' @description Detecte dans quel departement se trouve le polygon en entree
#'
#' @importFrom sf st_intersection st_transform st_crs
#'
#' @return Renvoi le numero de departement du polygon en entree
#' @export
#'
detectDepartement = function(shape){

   if (dim(shape)[1] != 1){
      stop(cat(paste0("La fonction prend un polygon unique en entree. ",
                      dim(shape)[1],
                      " polygons ont ete renseignes. Utiliser la fonction cadastreAnalysis::keepOutline() pour garder un unique polygon\n\n")))
   }

   shape = st_transform(shape,st_crs(data_departement))

   # benchmark realise avec st_crop
   num_dep = st_intersection(shape,data_departement)

   return(num_dep[["code_insee"]])

}
