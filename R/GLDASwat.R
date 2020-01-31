###3/11/19
#' Generate SWAT air temperature input files as well as air temperature stations file from NASA GLDAS remote sensing products.
#'
#' This function downloads remote sensing data of \acronym{GLDAS} from \acronym{NASA} \acronym{GSFC} servers, extracts air temperature data from grids within a specified watershed shapefile, and then generates tables in a format that \acronym{SWAT} requires for minimum and maximum air temperature data input. The function also generates the air temperature stations file input (file with columns: ID, File NAME, LAT, LONG, and ELEVATION) for those selected grids that fall within the specified watershed.
#' @param Dir A directory name to store gridded air temperature and air temperature stations files.
#' @param watershed A study watershed shapefile spatially describing polygon(s) in a geographic projection sp::CRS('+proj=longlat +datum=WGS84').
#' @param DEM A study watershed digital elevation model raster in a geographic projection sp::CRS('+proj=longlat +datum=WGS84').
#' @param start Begining date for gridded air temperature data.
#' @param end Ending date for gridded air temperature data.
#' @details A user should visit \url{https://disc.gsfc.nasa.gov/data-access} to register with the Earth Observing System Data and Information System (\acronym{NASA Earthdata}) and then authorize \acronym{NASA} GESDISC Data Access to successfuly work with this function. The function accesses \acronym{NASA} Goddard Space Flight Center server address for \acronym{GLDAS} remote sensing data products at (\url{https://hydro1.gesdisc.eosdis.nasa.gov/data/GLDAS/GLDAS_NOAH025_3H.2.1/}).  The function uses varible name ('Tair_f_inst') for air temperature in \acronym{GLDAS} data products. Units for gridded air temperature data are degrees in 'K'. The \command{GLDASwat} function outputs gridded air temperature (maximum and minimum) data in degrees 'C'.
#'
#' The goal of the Global Land Data Assimilation System \acronym{GLDAS} is to ingest satellite and ground-based observational data products, using advanced land surface modeling and data assimilation techniques, in order to generate optimal fields of land surface states and fluxes (Rodell et al., 2004). \acronym{GLDAS} dataset used in this function is the \acronym{GLDAS} Noah Land Surface Model L4 3 hourly 0.25 x 0.25 degree V2.1. The full suite of \acronym{GLDAS} datasets is avaliable at \url{https://hydro1.gesdisc.eosdis.nasa.gov/dods/}.  The \command{GLDASwat} finds the minimum and maximum air temperatures for each day at each grid within the study watershed by searching for minima and maxima over the three hours air temperature data values available for each day and grid.
#'
#' The \command{GLDASwat} function relies on 'curl' tool to transfer data from \acronym{NASA} servers to a user machine, using HTTPS supported protocol.  The 'curl' command embedded in this function to fetch \acronym{GLDAS} netcdf daily global files is designed to work seamlessly given that appropriate logging information are stored in the ".netrc" file and the cookies file ".urs_cookies" as explained in registering with the Earth Observing System Data and Information System. It is imperative to say here that a user machine should have 'curl' installed as a prerequisite to run \command{GLDASwat}.
#'
#' The \acronym{GLDAS} V2.1 simulation started on January 1, 2000 using the conditions from the \acronym{GLDAS} V2.0 simulation. The \acronym{GLDAS} V2.1 simulation was forced with National Oceanic and Atmospheric Administration \acronym{NOAA}, Global Data Assimilation System \acronym{GDAS} atmospheric analysis fields (Derber et al., 1991), the disaggregated Global Precipitation Climatology Project \acronym{GPCP} precipitation fields (Adler et al., 2003), and the Air Force Weather Agency’s AGRicultural METeorological modeling system \acronym{AGRMET} radiation fields which became available for March 1, 2001 onwards.
#' @note
#' \command{start} should be equal to or greater than 2000-Jan-01.
#' @author Ibrahim Mohammed, \email{ibrahim.mohammed@@nasa.gov}
#' @keywords NASA GLDAS Air Temperature
#' @return A table that includes points ID, Point file name, Lat, Long, and Elevation information formated to be read with \acronym{SWAT}, and
#' a scalar of maximum and minimum air temperature gridded data values at each point within the study watershed in ascii format needed by \acronym{SWAT} model weather inputs will be stored at \code{Dir}.
#' @references Derber, J. C., D. F. Parrish, and S. J. Lord (1991), The New Global Operational Analysis System at the National Meteorological Center, Weather Forecast, 6, 538-547, doi:10.1175/1520-0434(1991)006<0538:tngoas>2.0.co;2.
#' @references Adler, R. F., G. J. Huffman, A. Chang, R. Ferraro, P.-P. Xie, J. Janowiak, B. Rudolf, U. Schneider, S. Curtis, D. Bolvin, A. Gruber, J. Susskind, P. Arkin, and E. Nelkin (2003), The Version-2 Global Precipitation Climatology Project (GPCP) Monthly Precipitation Analysis (1979–Present), J. Hydrometeorol., 4, 1147-1167, doi:10.1175/1525-7541(2003)004<1147:tvgpcp>2.0.co;2.
#' @references Rodell, M., P. R. Houser, U. Jambor, J. Gottschalck, K. Mitchell, C.-J. Meng, K. Arsenault, B. Cosgrove, J. Radakovich, M. Bosilovich, J. K. Entin*, J. P. Walker, D. Lohmann, and D. Toll (2004), The Global Land Data Assimilation System, B. Am. Meteorol. Soc., 85, 381-394, doi:10.1175/bams-85-3-381.
#' @examples
#' #Lower Mekong basin example
#' \donttest{GLDASwat(Dir = "./SWAT_INPUT/", watershed = "LowerMekong.shp",
#' DEM = "LowerMekong_dem.tif", start = "2015-12-1", end = "2015-12-3")}
#' @import ncdf4 httr stringr rgdal XML utils sp
#' @importFrom stats na.exclude
#' @importFrom raster raster cellFromPolygon xyFromCell rowColFromCell extract
#' @export

GLDASwat=function(Dir='./SWAT_INPUT/', watershed ='LowerMekong.shp', DEM = 'LowerMekong_dem.tif', start = '2015-12-1', end = '2015-12-3')
{
  if(file.exists('~/.netrc')==TRUE||file.exists('~/_netrc')==TRUE)
  {
    if(length(grep("urs.earthdata.nasa.gov", readLines('~/.netrc')))==!0||length(grep("urs.earthdata.nasa.gov", readLines('~/_netrc')))==!0)
    {
  url.GLDAS.input <- 'https://hydro1.gesdisc.eosdis.nasa.gov/data/GLDAS/GLDAS_NOAH025_3H.2.1'
  myvar <- 'Tair_f_inst'
  ####Before getting to work on this function do this check
  if (as.Date(start) >= as.Date('2000-01-01'))
  {
    # Constructing time series based on start and end input days!
    time_period <- seq.Date(from = as.Date(start), to = as.Date(end), by = 'day')

    # Reading cell elevation data (DEM should be in geographic projection)
    watershed.elevation <- raster::raster(DEM)

    # Reading the study Watershed shapefile
    polys <- rgdal::readOGR(dsn=watershed,verbose = F)

    # SWAT climate 'air temperature' master file name
    filenametableKEY<-paste(Dir,'temp_Master.txt',sep='')

    # Creating empty lists
    filenameSWAT     <- list()
    filenameSWAT_TXT <- list()
    SubdailyTemp     <- list()
    cell.temp.values <- list()
    # The GLDAS data grid information
    # Read start day to extract spatial information and assign elevation data to the grids within the study watersheds

    julianDate  <- format(as.Date(start),format='%j')
    year <- format(as.Date(start),format='%Y')
    myurl = paste(paste(url.GLDAS.input,year,julianDate,sep = '/'),'/',sep = '')
    if(httr::status_code(GET(myurl))==200)
    {
      r <- httr::GET(myurl)
      filenames <- httr::content(r, "text")
      filenames <- XML::readHTMLTable(XML::htmlParse(filenames))[[1]]#getting the sub daily files at each juilan URL
      filenames <- unique(stats::na.exclude(stringr::str_extract(as.character(filenames$Name),'GLDAS.+(.nc4)')))
      # Extract the GLDAS nc4 files for the specific day
      # downloading one file to be able writing Climate info table and gridded file names
      if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
      utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[1],sep = ''),destfile = paste('./temp/',filenames[1],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')
      #reading ncdf file
      nc<-ncdf4::nc_open( paste('./temp/',filenames[1],sep = '') )
      #since geographic info for all files are the same (assuming we are working with the same data product)
      ###evaluate these values one time!
      ###getting the y values (longitudes in degrees east)
      nc.long<-ncdf4::ncvar_get(nc,nc$dim[[1]])
      ####getting the x values (latitudes in degrees north)
      nc.lat<-ncdf4::ncvar_get(nc,nc$dim[[2]])
      #extract data
      data<-matrix(as.vector(ncdf4::ncvar_get(nc,nc$var[[33]])),nrow=length(nc.lat),ncol=length(nc.long),byrow=T)
      #reorder the rows
      data<-data[ nrow(data):1, ]
      ncdf4::nc_close(nc)
      ###save the daily climate data values in a raster
      GLDAS<-raster::raster(x=data,xmn=nc.long[1],xmx=nc.long[NROW(nc.long)],ymn=nc.lat[1],ymx=nc.lat[NROW(nc.lat)],crs=sp::CRS('+proj=longlat +datum=WGS84'))
      #obtain cell numbers within the GLDAS raster
      cell.no<-raster::cellFromPolygon(GLDAS, polys)
      #obtain lat/long values corresponding to watershed cells
      cell.longlat<-raster::xyFromCell(GLDAS,unlist(cell.no))
      cell.rowCol <- raster::rowColFromCell(GLDAS,unlist(cell.no))
      points_elevation<-raster::extract(x=watershed.elevation,y=cell.longlat,method='simple')
      study_area_records<-data.frame(ID=unlist(cell.no),cell.longlat,cell.rowCol,Elevation=points_elevation)
      #sp::coordinates (study_area_records)<- ~x+y
      rm(data,GLDAS)
      unlink(x='./temp', recursive = TRUE)





    }



    #### Begin writing SWAT climate input tables
    #### Get the SWAT file names and then put the first record date
    for(jj in 1:dim(study_area_records)[1])
    {
      if(dir.exists(Dir)==FALSE){dir.create(Dir,recursive = TRUE)}
      filenameSWAT[[jj]]<-paste('temp',study_area_records$ID[jj],sep='')
      filenameSWAT_TXT[[jj]]<-paste(Dir,filenameSWAT[[jj]],'.txt',sep='')
      #write the data begining date once!
      write(x=format(time_period[1],'%Y%m%d'),file=filenameSWAT_TXT[[jj]])
    }


    #### Write out the SWAT grid information master table
    OutSWAT<-data.frame(ID=study_area_records$ID,NAME=unlist(filenameSWAT),LAT=study_area_records$y,LONG=study_area_records$x,ELEVATION=study_area_records$Elevation)
    utils::write.csv(OutSWAT,filenametableKEY,row.names = F,quote = F)

    #### Start doing the work!
    #### iterate over days to extract records at GLDAS grids estabished in 'study_area_records'


    for(kk in 1:length(time_period))
    {
      julianDate  <- format(as.Date(time_period[kk]),format='%j')
      year <- format(time_period[kk],format='%Y')
      myurl = paste(paste(url.GLDAS.input,year,julianDate,sep = '/'),'/',sep = '')
      r <- httr::GET(myurl)
      filenames <- httr::content(r, "text")
      filenames <- XML::readHTMLTable(XML::htmlParse(filenames))[[1]]#getting the subdaily files at each daily URL
      filenames <- unique(stats::na.exclude(stringr::str_extract(as.character(filenames$Name),'GLDAS.+(.nc4)')))
      # Extract the ncdf files
      for(ll in 1:length(filenames))# Iterating over each subdaily data file
      {

        # Downloading the file
        if(dir.exists('./temp/')==FALSE)
        {dir.create('./temp/')}
        utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[ll],sep = ''),destfile = paste('./temp/',filenames[ll],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')
        # Reading the ncdf file
        nc<-ncdf4::nc_open( paste('./temp/',filenames[ll],sep = '') )
        data<-matrix(as.vector(ncdf4::ncvar_get(nc,nc$var[[33]])),nrow=length(nc.lat),ncol=length(nc.long),byrow=T)
        # Reorder the rows
        data<-data[ nrow(data):1, ]
        ncdf4::nc_close(nc)
        ### Save the subdaily climate data values in a raster
        GLDAS<-raster::raster(x=data,xmn=nc.long[1],xmx=nc.long[NROW(nc.long)],ymn=nc.lat[1],ymx=nc.lat[NROW(nc.lat)],crs=sp::CRS('+proj=longlat +datum=WGS84'))
        ### Obtaining subdaily climate values at GLDAS grids that has been defined and explained earlier
        cell.values<-as.vector(GLDAS)[study_area_records$ID]
        SubdailyTemp[[ll]]<-cell.values

      }



      ###daily records for each point
      dailytemp<-matrix(unlist(SubdailyTemp),ncol=length(filenames),nrow = dim(study_area_records)[1])
      ###obtain minimum daily data over the 3 hrs records
      mindailytemp<-apply(dailytemp,1,min)
      mindailytemp<-mindailytemp - 273.16 #convert to degree C
      mindailytemp[is.na(mindailytemp)] <- '-99.0' #filling missing data
      ###same for maximum daily
      maxdailytemp<-apply(dailytemp,1,max)
      maxdailytemp<-maxdailytemp - 273.16 #convert to degree C
      maxdailytemp[is.na(maxdailytemp)] <- '-99.0' #filing missing data
      ### Looping through the GLDAS points and writing out the daily climate data in SWAT format
      for(k in 1:dim(study_area_records)[1])
      {
        cell.temp.values[[k]]<-paste(maxdailytemp[k],mindailytemp[k],sep=',')
        write(x=cell.temp.values[[k]],filenameSWAT_TXT[[k]],append=T,ncolumns = 1)
      }

      #empty memory and getting ready for next day!
      cell.temp.values<-list()
      SubdailyTemp<- list()
      rm(data,GLDAS)
      unlink(x='./temp', recursive = TRUE)
    }



  }


  else
  {
    cat('Sorry',paste(format(as.Date(start),'%b'),format(as.Date(start),'%Y'),sep=','),'is out of coverage for GLDAS data products.','  \n')
    cat('Please pick start date equal to or greater than 2000-Jan-01 to access GLDAS data products.','  \n')
    cat('Thank you!','  \n')
  }

    }
  }
  else
  {
    cat('Sorry!','  \n')
    cat('You need to create one/two file(s) named ".netrc" , "_netrc" and ".urs_cookies" at your home Directory. The "_netrc" file only needed for Windows users.','  \n')
    cat('Instructions on creating the ".netrc" and the ".urs_cookies" files can be accessed at https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget','  \n')
    cat('For Windows users follow instructions on creating the "_netrc" file at https://github.com/imohamme/NASAaccess/wiki/Curl-installation-on-Windows','  \n')
    cat('Make sure that the netrc file contain the follwoing line with your credentials: ','  \n')
    cat('machine urs.earthdata.nasa.gov login uid_goes_here password password_goes_here','  \n')
    cat('Thank you.','  \n')
  }
}
