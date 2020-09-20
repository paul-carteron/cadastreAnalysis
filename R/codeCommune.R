#' codeCommune
#'
#' @return Les codes communes associés aux parcellaires
#' @export
#'
#' @import dplyr here
#'
#' @examples
#'
codeCommune <-function(){

   codeComm = read_excel(here("parcCad.xlsx")) %>%
      select(commune) %>%
      unique() %>%
      pull()

   print(paste("Numeros de communes concerne(s) :",paste(codeComm, collapse = ", ")))
   return(codeComm)
}
