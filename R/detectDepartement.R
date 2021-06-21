#' detectDepartement
#'
#' @param shape Objet sf contenant un seul polygone
#'
#' @description Detecte dans quel departement se trouve le polygon en entree
#'
#' @importFrom sf st_intersects
#'
#' @return Renvoi le numero de departement du polygon en entree
#' @export
#'
detectDepartement = function(shape){

   if (dim(shape)[1] != 1){
      return(cat(paste0("La fonction prend un polygon unique en entree. ",
                        dim(shape)[1],
                        " polygons ont ete renseignes. \n\n")))
   }

   # benchmark realise avec st_crop
   num_dep = st_intersects(shape,data_departement)[[1]]

   return(num_dep)

}
