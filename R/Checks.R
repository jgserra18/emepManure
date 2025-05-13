## Read Animal class

mass_balance_checks = function(in_arg,
                  out_arg,
                  step) {


  if (step==1) { chk = sum(in_arg)-out_arg } # check 1; Exc_housing + Exc_yards + Exc_grazing = Exc_total
  if (step==2) { chk = sum(in_arg)-out_arg }


  if (chk!=0) {
    stop(paste0('Mass balance needs to be 0; it was ',checks,'\nInventory stopped.'))
  }
}



#' mbc_1
#' Mass balance check
#' @param exc_total total N excreted
#' @param list_exc_allocation list with N excreted onto different stages (housing, yards, grazing)
#' @description
#' mass balance check between total N excreted and N excreted onto different stages
#'
#' @returns
#' @export
#'
#' @examples examples(10, c(1,3,6))
mbc_1 = function(exc_total, list_exc_allocation) {

  chk = sum(list_exc_allocation) - exc_total
  chk
}

mbc_2 = function(Housing_solid_TAN,
                 Housing_solid_N
                 ) {


}
