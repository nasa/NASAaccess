###12/28/23
#' NASA GPM Near Real Time rainfall data
#'
#' This function downloads rainfall remote sensing data of \acronym{IMERG} from \acronym{NASA} \acronym{GSFC} servers, extracts data from grids within a specified watershed shapefile, and then generates tables in a format that any hydrological model requires for rainfall data input. The function also generates the rainfall stations file input (file with columns: ID, File NAME, LAT, LONG, and ELEVATION) for those selected grids that fall within the specified watershed. The minimum latency for this function is one day.
#' @param Dir A directory name to store gridded rainfall and rain stations files.
#' @param watershed A study watershed shapefile spatially describing polygon(s) in a geographic projection crs='+proj=longlat +datum=WGS84'.
#' @param DEM A study watershed digital elevation model raster in a geographic projection crs='+proj=longlat +datum=WGS84'.
#' @param start Beginning date for gridded rainfall data.
#' @param end Ending date for gridded rainfall data.
#' @details A user should visit \url{https://disc.gsfc.nasa.gov/information/documents} Data Access document to register with the Earth Observing System Data and Information System (\acronym{NASA Earthdata}) and then authorize \acronym{NASA} GESDISC Data Access to successfully work with this function. The function accesses \acronym{NASA} Goddard Space Flight Center server address for \acronym{IMERG} remote sensing data products at (\url{https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDE.06/}).  The function uses variable name ('precipitationCal') for rainfall in \acronym{IMERG} data products. Units for gridded rainfall data are 'mm'.
#'
#' \acronym{IMERG} dataset is the GPM Level 3 \acronym{IMERG} *Early* Daily 0.1 x 0.1 deg (GPM_3IMERGDE) derived from the half-hourly \acronym{GPM_3IMERGHHE}. The derived result represents the final estimate of the daily accumulated precipitation. The dataset is produced at the \acronym{NASA} Goddard Earth Sciences (GES) Data and Information Services Center (DISC) by simply summing the valid precipitation retrievals for the day in GPM_3IMERGHHE and giving the result in (mm) \url{https://gpm.nasa.gov/data/directory}.
#'
#' The \acronym{IMERG} data products are available from 2000-June-1 to present.
#' The function outputs table and gridded data files that match grid points resolution of \acronym{IMERG} data products (i.e., resolution of 0.1 deg).
#'
#' The \command{GPM_NRT} function relies on 'curl' tool to transfer data from \acronym{NASA} servers to a user machine, using HTTPS supported protocol.  The 'curl' command embedded in this function to fetch precipitation \acronym{IMERG} netcdf daily global files is designed to work seamlessly given that appropriate logging information are stored in the ".netrc" file and the cookies file ".urs_cookies" as explained in registering with the Earth Observing System Data and Information System. It is imperative to say here that a user machine should have 'curl' installed as a prerequisite to run \command{GPM_NRT}.
#' @note
#' \command{start} should be equal to or greater than 2000-Jun-01.
#'
#' \command{end} the minimum latency is 1 day.
#' @author Ibrahim Mohammed, \email{ibrahim.mohammed@@nasa.gov}
#' @keywords NASA IMERG Near Real Time NRT Precipitation
#' @return A table that includes points ID, Point file name, Lat, Long, and Elevation information, and
#' a scalar of rainfall gridded data values at each point within the study watershed in ascii format needed by hydrological model weather inputs will be stored at \code{Dir}.
#' @examples
#' #Lower Mekong basin example
#' \dontrun{GPM_NRT(Dir = "./INPUT/", watershed = "LowerMekong.shp",
#' DEM = "LowerMekong_dem.tif", start = "2022-6-1", end = "2022-6-10")}
#' @import ncdf4 httr stringr utils XML methods getPass
#' @importFrom stats na.exclude
#' @export



GPM_NRT=function(Dir='./INPUT/', watershed ='LowerMekong.shp', DEM = 'LowerMekong_dem.tif', start = '2022-6-1', end = '2022-6-10')
{

  if(file.exists('~/.netrc')==FALSE)
  {

    source(system.file("scripts", "netrc.R",
                package = "NASAaccess"))
  }


  if(file.exists('~/.netrc')==TRUE)
  {
    if(length(grep("urs.earthdata.nasa.gov", readLines('~/.netrc')))==!0)
    {


        url.IMERG.input <- 'https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDE.06/'
        myvarIMERG <- 'precipitationCal'
        #check the GPM IMERG server availability
        if(httr::status_code(GET(url.IMERG.input))==200)
        {
          ####Before getting to work on this function do this check
          if (as.Date(start) >= as.Date('2000-06-01'))
          {
            # Constructing time series based on start and end input days!
            time_period <- seq.Date(from = as.Date(start), to = as.Date(end), by = 'day')

            # Reading cell elevation data (DEM should be in geographic projection)
            watershed.elevation <- terra::rast(DEM)

            # Reading the study Watershed shapefile
            polys <- terra::vect(watershed)

            # weather 'precipitation' master file name
            filenametableKEY<-paste(Dir,'precipitation','Master.txt',sep='')

            # Creating empty lists
            filenameMODEL     <- list()
            filenameMODEL_TXT <- list()



            # The IMERG data grid information
            # Read the start day first to extract spatial information and assign elevation data to the grids within the study watersheds

            #DUMMY_DATE <- as.Date('2014-05-01')

            mon  <- format(time_period[1],format='%m')
            year <- format(time_period[1],format='%Y')
            myurl = paste(paste(url.IMERG.input,year,mon,sep = '/'),'/',sep = '')
            if(httr::status_code(GET(myurl))==200)
            {
              r <- httr::GET(myurl)
              filenames <- httr::content(r, "text")
              filenames <- XML::readHTMLTable(XML::htmlParse(filenames))[[1]]#getting the daily files at each monthly URL
              filenames <- unique(stats::na.exclude(stringr::str_extract(as.character(filenames$Name),'3B-DAY.+(.nc4)')))
              # Extract the IMERG nc4 files for the specific month
              # trying here the first day since I am only interested on grid locations
              # downloading one file
              if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
              utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[1],sep = ''),destfile = paste('./temp/',filenames[1],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')
              test1<-file.info(paste('./temp/',filenames[1],sep= ''))$size
              stopifnot('The GPM IMERG server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test1 > 3.0e6)
              #reading ncdf file
              nc<-ncdf4::nc_open( paste('./temp/',filenames[1],sep = '') )
              #since geographic info for all files are the same (assuming we are working with the same data product)
              ###evaluate these values one time!
              ###getting the y values (longitudes in degrees east)
              nc.long.IMERG<-ncdf4::ncvar_get(nc,nc$dim[[1]])
              ####getting the x values (latitudes in degrees north)
              nc.lat.IMERG<-ncdf4::ncvar_get(nc,nc$dim[[2]])
              # create a raster
              IMERG<-terra::rast(nrows=length(nc.lat.IMERG),
                                 ncols=length(nc.long.IMERG),
                                 xmin=nc.long.IMERG[1],
                                 xmax=nc.long.IMERG[NROW(nc.long.IMERG)],
                                 ymin=nc.lat.IMERG[1],
                                 ymax=nc.lat.IMERG[NROW(nc.lat.IMERG)],
                                 crs='+proj=longlat +datum=WGS84')

              #fill raster with dummy values

              values(IMERG) <- 1:ncell(IMERG)

              ncdf4::nc_close(nc)
              # Convert raster to points
              IMERG.points <- terra::as.points(IMERG, na.rm = TRUE)

              # Intersect to keep only points on the shape
              IMERG.points <- IMERG.points[polys]
              #obtain cell numbers within the IMERG raster
              cell.no <- terra::cells(IMERG, IMERG.points)[,2]
              #obtain lat/long values corresponding to watershed cells
              cell.longlat<-terra::xyFromCell(IMERG,cell.no)


              cell.rowCol <- terra::rowColFromCell(IMERG,cell.no)


              points_elevation<-terra::extract(x=watershed.elevation,y=cell.longlat,method='simple')

              FinalTable<-data.frame(ID=unlist(cell.no),cell.longlat,cell.rowCol,Elevation=points_elevation[,])



              rm(IMERG)

            }



            #### Begin writing weather input tables
            #### Get the file names and then put the first record date
            for(jj in 1:dim(FinalTable)[1])
            {
              if(dir.exists(Dir)==FALSE){dir.create(Dir,recursive = TRUE)}
              filenameMODEL[[jj]]<-paste('precipitation',FinalTable$ID[jj],sep='')
              filenameMODEL_TXT[[jj]]<-paste(Dir,filenameMODEL[[jj]],'.txt',sep='')
              #write the data beginning date once!
              write(x=format(time_period[1],'%Y%m%d'),file=filenameMODEL_TXT[[jj]])
            }


            #### Write out the grid information master table
            OutTable<-data.frame(ID=FinalTable$ID,NAME=unlist(filenameMODEL),LAT=FinalTable$y,LONG=FinalTable$x,ELEVATION=FinalTable$Elevation)
            utils::write.csv(OutTable,filenametableKEY,row.names = F,quote = F)




            #### Start doing the work!
            #### iterate over days to extract record at IMERG grids established in 'FinalTable'


            for(kk in 1:length(time_period))
            {
              mon  <- format(time_period[kk],format='%m')
              year <- format(time_period[kk],format='%Y')
              myurl = paste(paste(url.IMERG.input,year,mon,sep = '/'),'/',sep = '')
                if(httr::status_code(GET(myurl))==200)
                {
                  r <- httr::GET(myurl)
                  filenames <- httr::content(r, "text")
                  filenames <- XML::readHTMLTable(XML::htmlParse(filenames))[[1]]# Getting the daily files at each monthly URL
                  filenames <- unique(stats::na.exclude(stringr::str_extract(as.character(filenames$Name),'3B-DAY.+(.nc4)')))
                  tt<-as.Date(stringr::str_extract(stringr::str_extract(filenames,"20.+-"),'[0-9]{1,8}'),format='%Y%m%d')
                  pp<-tt%in%time_period[kk]
                  filenames<-filenames[pp]
                  # Extract the ncdf files

                  for(ll in 1:length(filenames))# Iterating over each daily data file
                  {
                    # Downloading the file
                    if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
                    if(file.exists(paste('./temp/',filenames[ll],sep= ''))==FALSE){utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[ll],sep = ''),destfile = paste('./temp/',filenames[ll],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')}
                    # Reading the ncdf file
                    nc<-ncdf4::nc_open( paste('./temp/',filenames[ll],sep = '') )
                    if(ll==1)
                    {
                      IMERG<-terra::rast(nrows=length(nc.lat.IMERG),
                                         ncols=length(nc.long.IMERG),
                                         xmin=nc.long.IMERG[1],
                                         xmax=nc.long.IMERG[NROW(nc.long.IMERG)],
                                         ymin=nc.lat.IMERG[1],
                                         ymax=nc.lat.IMERG[NROW(nc.lat.IMERG)],
                                         crs='+proj=longlat +datum=WGS84')
                    }

                    ###save the daily weather data values in a raster
                    values(IMERG) <- ncdf4::ncvar_get(nc,myvarIMERG)

                    # Reorder the rows

                    IMERG <- terra::flip(IMERG,direction="v")

                    ncdf4::nc_close(nc)

                    #obtain daily weather values at cells bounded with the study watershed (extract values from a raster)

                    cell.values<-extract(IMERG,FinalTable$ID)[,]
                    cell.values[is.na(cell.values)] <- '-99.0' #filling missing data



                    #loop through the grid points to write out the daily weather data
                    for(jj in 1:dim(FinalTable)[1])
                    {
                      write(x=cell.values[jj],filenameMODEL_TXT[[jj]],append=T,ncolumns = 1)
                    }

                  }
                  unlink(x='./temp', recursive = TRUE)
                }







            }

          }
          else
          {
            cat('Sorry',paste(format(as.Date(start),'%b'),format(as.Date(start),'%Y'),sep=','),'is out of coverage for IMERG data products.','  \n')
            cat('Please pick start date equal to or greater than 2000-Jun-01 to access IMERG data products.','  \n')
            cat('Thank you!','  \n')
          }
        }

        else
        {
          cat('Sorry!','  \n')
          cat('The GPM IMERG Early Precipitation L3 server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.','  \n')
          cat('Thank you.','  \n')
        }

    }




  }
  else
  {
    cat('Sorry!','  \n')
    cat('You need to create two files ".netrc" and ".urs_cookies" at your home Directory.','  \n')
    cat('Instructions on creating the ".netrc" and the ".urs_cookies" files can be accessed at https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget','  \n')
    cat('Make sure that the ".netrc" file contains the follwoing line with your credentials: ','  \n')
    cat('machine urs.earthdata.nasa.gov login uid_goes_here password password_goes_here','  \n')
    cat('Thank you.','  \n')
  }
}
