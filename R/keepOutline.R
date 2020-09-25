#' Outline
#'
#' @param parcelles L'objet sf contenant les parcelles
#'
#' @return Le contour de nos parcelles. Format sortie : Lambert93 / EPSG : 2154
#' @export
#'
#' @importFrom R.utils gunzip
#' @importFrom rmapshaper ms_simplify
#' @importFrom nngeo st_remove_holes
#'
#' @import sf
#'
keepOutline <- function(parcelles) {

   res = parcelles %>%
      st_transform(2154) %>%
      ms_simplify(keep_shapes = 1, snap_interval = 20) %>%
      st_union() %>%
      st_remove_holes() %>%
      st_make_valid() %>%
      st_sf() %>%
      st_transform(4326)

   return(res)
}
