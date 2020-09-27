#' courbeNiveau
#'
#' @param MNT Fichier MNT obtenue avec la fonction importMNT
#' @param distInterCourbe Altitude enter deux courbes de niveaux
#'
#' @return Renvoi les courbes de niveau a partir d'un MNT
#' @export
#'
#' @import sf stars
#'
courbeNiveau <- function(MNT, distInterCourbe = 100) {

  MNT = st_as_stars(MNT)
  ext = round(range(MNT$layer, na.rm = TRUE),-2)
  brk = seq(ext[1], ext[2]-distInterCourbe, by = distInterCourbe)

  print(paste("Courbe max :",max(brk),"m -", "Courbe min :", min(brk),"m"))

  courbeNiv = st_contour(MNT,
                         breaks = brk) %>% st_transform(4326)

  return(courbeNiv)

  }
