#' Convert a netCDF to a stars object with a curvilinear grid
#'
#' @param fp A netCDF filepath with a curvilinear grid.
#' @param var_name A string matching the variable inside of the NetCDF
#' @param lat_name A string matching the name of latitude in the NetCDF
#' @param long_name A string matching the name of longitude in the NetCDF
#' @param time_name A string matching the name of time in the NetCDF
#'
#' @return A curvilinear `stars` object.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- curvilinear_nc("hourlyScab/conus404/wrf2d_d01_1996-08-30_170000.nc","T2")
#' }
curvilinear_nc <- function(fp,var_name,lat_name = "XLAT",long_name = "XLONG",time_name = "XTIME"){

  #Open NC File
  nc_in <- ncdf4::nc_open(fp)

  #Read in Latitude Longitude and Time
  lat <- ncdf4::ncvar_get(nc_in,lat_name)
  long <- ncdf4::ncvar_get(nc_in,long_name)
  time <- ncdf4::ncvar_get(nc_in,time_name)

  #Read in Variable(s)
  var <- ncdf4::ncvar_get(nc_in,var_name)

  #CHECK FOR TRANSPOSED LAT
  if (abs(lat[,1][1] - lat[,1][length(lat[,1])]) < 10){
    #THIS IMPLIES THE DATA IS TRANSPOSED.THERE SHOULD BE A LARGE GRADIENT HERE
    #UNTRANSPOSE ALL DATA
    lat <- t(lat)
    long <- t(long)
    time <- t(time)
    var <- t(var)
  }
  #CHECK FOR FLIPPED LAT
  if (lat[,1][1] - lat[,1][length(lat[,1])] < 0){
    #Implies the data needs a vertical flip
    lat <- lat[nrow(lat):1, ]
    long <- long[nrow(long):1, ]
    time <- time[nrow(time):1, ]
    var <- var[nrow(var):1, ]
  }

  s0 = stars::st_as_stars(var)
  s = stars::st_as_stars(
    s0,
    curvilinear = list(
      X1 = long,  # same name as the first dimension
      X2 = lat   # same name as the second dimension
    )
  )
  ncdf4::nc_close(nc_in)

  return(s)
}
