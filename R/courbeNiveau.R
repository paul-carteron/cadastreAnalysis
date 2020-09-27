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
courbeNiveau <- function(MNT) {

  MNT = st_as_stars(MNT)
  ext = range(MNT$layer, na.rm = TRUE)

  if(abs(ext[2]-ext[1]) < 100){
    ext = round(range(MNT$layer, na.rm = TRUE),-1)
    distInterCourbe = 5
  }else if(abs(ext[2]-ext[1]) < 250){
    ext = round(range(MNT$layer, na.rm = TRUE),-1)
    distInterCourbe = 10
  }else if(abs(ext[2]-ext[1]) < 500){
    ext = round(range(MNT$layer, na.rm = TRUE),-1)
    distInterCourbe = 20
  }else if(abs(ext[2]-ext[1]) < 1000){
    ext = round(range(MNT$layer, na.rm = TRUE),-1)
    distInterCourbe = 50
  }else{
    ext = round(range(MNT$layer, na.rm = TRUE),-2)
    distInterCourbe = 100
  }

  brk = seq(ext[1], ext[2]-distInterCourbe, by = distInterCourbe)

  print(paste("Courbe max :",max(brk),"m -", "Courbe min :", min(brk),"m"))
  print(paste("La distance entre deux courbes de niveau est de :",distInterCourbe,"m"))

  courbeNiv = st_contour(MNT,
                         breaks = brk) %>% st_transform(4326)

  return(courbeNiv)

  }
