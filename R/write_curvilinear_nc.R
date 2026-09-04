#' Convert a netCDF to a stars object with a curvilinear grid
#'
#' @param out_fp A netCDF filepath with a curvilinear grid.
#' @param var A string matching the variable inside of the NetCDF
#' @param lat A string matching the name of latitude in the NetCDF
#' @param long A string matching the name of longitude in the NetCDF
#' @param time A string matching the name of time in the NetCDF
#' @param var_name A string matching the variable inside of the NetCDF
#' @param var_units A string matching the variable inside of the NetCDF
#' @param lat_name A string matching the name of latitude in the NetCDF
#' @param long_name A string matching the name of longitude in the NetCDF
#' @param time_name A string matching the name of time in the NetCDF
#' @param time_units String defining time units
#'
#' @return A curvilinear `stars` object.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- curvilinear_nc("hourlyScab/conus404/wrf2d_d01_1996-08-30_170000.nc","T2")
#' }

write_curvilinear_nc <- function(out_fp, var, lat, long, time, var_name="A1", var_units = "undefined",
                                 lat_name = "XLAT", long_name = "XLONG", time_name = "XTIME",time_units = "minutes since 1970-01-01 00:00:00"){

  we <- dim(var)[1]
  sn <- dim(var)[2]

  lat_vals <- lat
  lon_vals <- long

  we <- dim(var)[1]
  sn <- dim(var)[2]

  #GET EAST WESTING
  west_east <- ncdf4::ncdim_def("west_east","",1:we,create_dimvar = FALSE)
  south_north <- ncdf4::ncdim_def('south_north',"",1:sn,create_dimvar=FALSE)
  time_time <- ncdf4::ncdim_def('Time',"",1:length(time),create_dimvar=FALSE)

  #DEFINE THE VARIABLES
  var_lat <- ncdf4::ncvar_def('XLAT',"degrees_north",list(west_east,south_north,time_time),prec = 'double')
  var_long <- ncdf4::ncvar_def('XLONG',"degrees_east",list(west_east,south_north,time_time),prec = 'double')
  var_time <- ncdf4::ncvar_def('XTIME',"timey",list(time_time),prec = 'double')

  #BUILD SUPPORT FOR MULTI VAR LATER
  var_var1 <- ncdf4::ncvar_def(var_name,var_units,list(west_east,south_north,time_time),prec = 'double')

  #CONSTRUCT AND OPEN THE NETCDF
  nc_out <- ncdf4::nc_create(out_fp,list(var_lat,var_long,var_var1,var_time))

  ### Now add the actual data in there, the shape should be appropriate for this allegedly
  ncdf4::ncvar_put(nc_out,"XLAT",lat_vals)
  ncdf4::ncvar_put(nc_out,"XLONG",lon_vals)
  ncdf4::ncvar_put(nc_out,"XTIME",1)
  ncdf4::ncvar_put(nc_out,var_name,var)



  ncdf4::nc_close(nc_out)

}

