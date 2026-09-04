#' download climate data
#'
#' @param model Gridded climate model names: "RTMA, URMA, CONSUS404, etc"
#' @param date Datetime
#'
#' @return downloads data, returns status
#' @export
#'
#' @examples
#' \dontrun{
#' r <- download_climate('RTMA',date,4,out_dir = ".")
#' }
#'

climate_fp <- function(model,date){

  options(timeout = max(300, getOption("timeout")))

  t <- trunc(as.POSIXlt(date, tz = "UTC"), units = "hours")

  #Identify the base model. This is so we don't do this every loop
  if(model == "CONUS404"){
    base <- "https://tds.gdex.ucar.edu/thredds/dodsC/files/g/d559000"
  }
  else if(model == "RTMA"){
    base <- "https://noaa-rtma-pds.s3.amazonaws.com"
  }
  else if(model == "ERA5"){
    print("ERROR: Not Setup Yet")
    return()
  }
  else{
    print("ERROR: Model not Recognized")
    return()
  }

  ###

  if(model == "CONUS404"){

    # WRF "water year" runs Oct 1 - Sep 30; Oct-Dec belongs to the *next* WY
    wy <- ifelse(lubridate::month(t) >= 10, lubridate::year(t) + 1, lubridate::year(t))
    yyyymm <- format(t, "%Y%m")
    fname  <- sprintf("wrf2d_d01_%s_%s:00:00.nc", format(t, "%Y-%m-%d"), format(t, "%H"))

    #"https://tds.gdex.ucar.edu/thredds/dodsC/files/g/d559000/wy1980/197910/wrf2d_d01_1979-10-01_03:00:00.nc"
    url <- sprintf("%s/wy%d/%s/%s", base, wy, yyyymm, fname)
  }

  else if(model == "URMA"){

    ymd <- format(t, "%Y%m%d", tz = "UTC")
    hh  <- format(t, "%H",     tz = "UTC")
    url <- sprintf("%s/urma2p5.%s/urma2p5.t%sz.2dvaranl_ndfd.grb2_wexp", base, ymd, hh)
  }

  else if(model == "RTMA"){

    ymd <- format(t, "%Y%m%d", tz = "UTC")
    hh  <- format(t, "%H",     tz = "UTC")
    url <- sprintf("%s/rtma2p5.%s/rtma2p5.t%sz.2dvaranl_ndfd.grb2_wexp", base, ymd, hh)
  }
  else{
    print("ERROR: Model not Recognized")
    return()
  }
  return(url)
}
