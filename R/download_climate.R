#' download climate data
#'
#' @param model Gridded climate model names: "RTMA, URMA, CONSUS404, etc"
#' @param date Datetime
#' @param hours Number of hours to be downloaded following the start time
#' @param out_dir Where the files should be downloaded
#'
#' @return downloads data, returns status
#' @export
#'
#' @examples
#' \dontrun{
#' r <- download_climate('RTMA',date,4,out_dir = ".")
#' }
#'
download_climate <- function(model,date,hours,out_dir = "."){

  options(timeout = max(300, getOption("timeout")))

  date <- trunc(as.POSIXlt(date, tz = "UTC"), units = "hours")
  times <- seq(date, by = "1 hour", length.out = hours)

  #Identify the base model. This is so we don't do this every loop
  if(model == "CONUS404"){
    base <- "https://tds.gdex.ucar.edu/thredds/fileServer/files/g/d559000"
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

  print(paste0("Downloading ",hours, ' hours of ',model,' data beginning on ',date))

  ###
  for (i in seq_along(times)) {

    Sys.sleep(1)
    t <- times[i]

    if(model == "CONUS404"){

      # WRF "water year" runs Oct 1 - Sep 30; Oct-Dec belongs to the *next* WY
      wy <- ifelse(lubridate::month(t) >= 10, lubridate::year(t) + 1, lubridate::year(t))
      yyyymm <- format(t, "%Y%m")
      fname  <- sprintf("wrf2d_d01_%s_%s:00:00.nc", format(t, "%Y-%m-%d"), format(t, "%H"))
      url <- sprintf("%s/wy%d/%s/%s", base, wy, yyyymm, fname)
      fname <- gsub(":","",fname)
      dst <- file.path(out_dir, fname)
    }

    else if(model == "URMA"){

      ymd <- format(t, "%Y%m%d", tz = "UTC")
      hh  <- format(t, "%H",     tz = "UTC")
      url <- sprintf("%s/urma2p5.%s/urma2p5.t%sz.2dvaranl_ndfd.grb2_wexp", base, ymd, hh)
      dst <- file.path(out_dir, sprintf("rtma2p5_%s_t%sz.grb2", ymd, hh))
    }

    else if(model == "RTMA"){

      ymd <- format(t, "%Y%m%d", tz = "UTC")
      hh  <- format(t, "%H",     tz = "UTC")
      url <- sprintf("%s/rtma2p5.%s/rtma2p5.t%sz.2dvaranl_ndfd.grb2_wexp", base, ymd, hh)
      dst <- file.path(out_dir, sprintf("rtma2p5_%s_t%sz.grb2", ymd, hh))

    }
    else{
      print("ERROR: Model not Recognized")
      return()
    }

    ###Download the file
    if(length(times) > 3){

      k <- (i/length(times)) * 100
      prog(k)

      if (file.exists(dst)){
        next
        }
      }

    else{
      if (file.exists(dst)){
        print(paste0('File Already Downloaded!!',dst))
        next
      }
      else{
        print(paste0('Downloading: ',dst))
      }
    }

    tryCatch(
      utils::download.file(url, dst, mode = "wb", quiet = TRUE),
      error = function(e) message("skip ", dst, " z: ", conditionMessage(e)))
    ###


  }
  print('Downloads Complete')
}

