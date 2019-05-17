###3/11/19
#' Generate SWAT rainfall input files as well as rain stations file from NASA GPM remote sensing products.
#'
#' This function downloads rainfall remote sensing data of \acronym{TRMM} and \acronym{IMERG} from \acronym{NASA} \acronym{GSFC} servers, extracts data from grids within a specified watershed shapefile, and then generates tables in a format that \acronym{SWAT} requires for rainfall data input. The function also generates the rainfall stations file input (file with columns: ID, File NAME, LAT, LONG, and ELEVATION) for those selected grids that fall within the specified watershed.
#' @param Dir A directory name to store gridded rainfall and rain stations files.
#' @param watershed A study watershed shapefile spatially describing polygon(s) in a geographic projection sp::CRS('+proj=longlat +datum=WGS84').
#' @param DEM A study watershed digital elevation model raster in a geographic projection sp::CRS('+proj=longlat +datum=WGS84').
#' @param start Begining date for gridded rainfall data.
#' @param end Ending date for gridded rainfall data.
#' @details A user should visit \url{https://disc.gsfc.nasa.gov/data-access} to register with the Earth Observing System Data and Information System (\acronym{NASA Earthdata}) and then authorize \acronym{NASA} GESDISC Data Access to successfuly work with this function. The function accesses \acronym{NASA} Goddard Space Flight Center server address for \acronym{IMERG} remote sensing data products at (\url{https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.05/}), and \acronym{NASA} Goddard Space Flight Center server address for \acronym{TRMM} remote sensing data products (\url{https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_RT/TRMM_3B42RT_Daily.7}).  The function uses varible name ('precipitationCal') for rainfall in \acronym{IMERG} data products and variable name ('precipitation') for \acronym{TRMM} rainfall data products. Units for gridded rainfall data are 'mm'.
#'
#' \acronym{IMERG} dataset is the GPM Level 3 \acronym{IMERG} *Final* Daily 0.1 x 0.1 deg (GPM_3IMERGDF) derived from the half-hourly GPM_3IMERGHH. The derived result represents the final estimate of the daily accumulated precipitation. The dataset is produced at the \acronym{NASA} Goddard Earth Sciences (GES) Data and Information Services Center (DISC) by simply summing the valid precipitation retrievals for the day in GPM_3IMERGHH and giving the result in (mm) \url{https://pmm.nasa.gov/data-access/downloads/gpm}.
#'
#' \acronym{TRMM} dataset is a daily 0.25 x 0.25 deg accumulated precipitation product that is generated from the Near Real-Time 3-hourly TMPA (3B42RT). It is produced at the NASA GES DISC, as a value added product. Simple summation of valid retrievals in a grid cell is applied for the data day. The result is given in (mm) \url{https://pmm.nasa.gov/data-access/downloads/trmm}.
#'
#' Since \acronym{IMERG} data products are only available from 2014-March-12 to present, then this function uses \acronym{TRMM} data products for time periods earlier than 2014-March-12. Keep in mind that \acronym{TRMM} data products that are compatible with \acronym{IMERG} data products are only available from 2000-March-01.
#' The function outputs table and gridded data files that match grid points resolution of \acronym{IMERG} data products (i.e., resolution of 0.1 deg). Since \acronym{TRMM} and \acronym{IMERG} data products do not have a similar spatial resolution (i.e., 0.25 and 0.1 deg respectively), the function assigns the nearest \acronym{TRMM} grid point to any missing \acronym{IMERG} data point as an approximate (i.e. during 2000-March-01 to 2014-March-11 time period).
#'
#' The \command{GPMswat} function relies on 'curl' tool to transfer data from \acronym{NASA} servers to a user machine, using HTTPS supported protocol.  The 'curl' command embedded in this function to fetch precipitation \acronym{IMERG}/\acronym{TRMM} netcdf daily global files is designed to work seamlessly given that appropriate logging information are stored in the ".netrc" file and the cookies file ".urs_cookies" as explained in registering with the Earth Observing System Data and Information System. It is imperative to say here that a user machine should have 'curl' installed as a prerequisite to run \command{GPMswat}.
#' @note
#' \command{start} should be equal to or greater than 2000-Mar-01.
#' @author Ibrahim Mohammed, \email{ibrahim.mohammed@@nasa.gov}
#' @keywords NASA IMERG TRMM Precipitation
#' @return A table that includes points ID, Point file name, Lat, Long, and Elevation information formated to be read with \acronym{SWAT}, and
#' a scalar of rainfall gridded data values at each point within the study watershed in ascii format needed by \acronym{SWAT} model weather inputs will be stored at \code{Dir}.
#' @examples
#' #Lower Mekong basin example
#' \donttest{GPMswat(Dir = "./SWAT_INPUT/", watershed = "LowerMekong.shp",
#' DEM = "LowerMekong_dem.tif", start = "2015-12-1", end = "2015-12-3")}
#' @import ncdf4 shapefiles rgeos maptools httr stringr rgdal XML utils sp methods
#' @importFrom stats na.exclude
#' @importFrom raster raster cellFromPolygon xyFromCell rowColFromCell extract
#' @export



GPMswat=function(Dir='./SWAT_INPUT/', watershed ='LowerMekong.shp', DEM = 'LowerMekong_dem.tif', start = '2015-12-1', end = '2015-12-3')
{

  if(file.exists('~/.netrc')==TRUE||file.exists('~/_netrc')==TRUE)
  {
    if(length(grep("urs.earthdata.nasa.gov", readLines('~/.netrc')))==!0||length(grep("urs.earthdata.nasa.gov", readLines('~/_netrc')))==!0)
    {


        url.IMERG.input <- 'https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/'
        url.TRMM.input <- 'https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_RT/TRMM_3B42RT_Daily.7'
        myvarIMERG <- 'precipitationCal'
        myvarTRMM <- 'precipitation'
        ####Before getting to work on this function do this check
        if (as.Date(start) >= as.Date('2000-03-01'))
          {
    # Constructing time series based on start and end input days!
    time_period <- seq.Date(from = as.Date(start), to = as.Date(end), by = 'day')

    # Reading cell elevation data (DEM should be in geographic projection)
    watershed.elevation <- raster::raster(DEM)

    # Reading the study Watershed shapefile
    polys <- rgdal::readOGR(dsn=watershed,verbose = F)

    # SWAT climate 'precipitation' master file name
    filenametableKEY<-paste(Dir,myvarTRMM,'Master.txt',sep='')

    # Creating empty lists
    filenameSWAT     <- list()
    filenameSWAT_TXT <- list()
    closestSiteVec   <- list()
    minDistVec       <- list()



    # The IMERG data grid information
    # Read a dummy day to extract spatial information and assign elevation data to the grids within the study watersheds

    DUMMY_DATE <- as.Date('2014-05-01')

    mon  <- format(DUMMY_DATE,format='%m')
    year <- format(DUMMY_DATE,format='%Y')
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
      #reading ncdf file
      nc<-ncdf4::nc_open( paste('./temp/',filenames[1],sep = '') )
      #since geographic info for all files are the same (assuming we are working with the same data product)
      ###evaluate these values one time!
      ###getting the y values (longitudes in degrees east)
      nc.long.IMERG<-ncdf4::ncvar_get(nc,nc$dim[[1]])
      ####getting the x values (latitudes in degrees north)
      nc.lat.IMERG<-ncdf4::ncvar_get(nc,nc$dim[[2]])
      #extract data
      data<-ncdf4::ncvar_get(nc,myvarIMERG)
      #reorder the rows
      data<-data[ nrow(data):1, ]
      ncdf4::nc_close(nc)
      ###save the daily climate data values in a raster
      IMERG<-raster::raster(x=as.matrix(data),xmn=nc.long.IMERG[1],xmx=nc.long.IMERG[NROW(nc.long.IMERG)],ymn=nc.lat.IMERG[1],ymx=nc.lat.IMERG[NROW(nc.lat.IMERG)],crs=sp::CRS('+proj=longlat +datum=WGS84'))
      #obtain cell numbers within the IMERG raster
      cell.no<-raster::cellFromPolygon(IMERG, polys)
      #obtain lat/long values corresponding to watershed cells
      cell.longlat<-raster::xyFromCell(IMERG,unlist(cell.no))
      cell.rowCol <- raster::rowColFromCell(IMERG,unlist(cell.no))
      points_elevation<-raster::extract(x=watershed.elevation,y=cell.longlat,method='simple')
      study_area_records_IMERG<-data.frame(ID=unlist(cell.no),cell.longlat,cell.rowCol,Elevation=points_elevation)
      sp::coordinates (study_area_records_IMERG)<- ~x+y
      rm(data,IMERG)
      unlink(x='./temp', recursive = TRUE)
    }

    # The TRMM data grid information
    # Use the same dummy date defined above since TRMM has data up to present with less accurancy. The recomendation is to use IMERG data from 2014-03-12 and onward!
    # update my url with TRMM information
    myurl = paste(paste(url.TRMM.input,year,mon,sep = '/'),'/',sep = '')

    if(httr::status_code(GET(myurl))==200)
    {
      r <- httr::GET(myurl)
      filenames <- httr::content(r, "text")
      filenames <- XML::readHTMLTable(XML::htmlParse(filenames))[[1]]# getting the daily files at each monthly URL
      filenames <- unique(stats::na.exclude(stringr::str_extract(as.character(filenames$Name),'3B42.+(.nc4)')))
      # Extract the TRMM nc4 files for the specific month
      # trying here the first day since I am only interested on grid locations
      # downloading one file
      if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
      utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[1],sep = ''),destfile = paste('./temp/',filenames[1],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')
      #reading ncdf file
      nc<-ncdf4::nc_open( paste('./temp/',filenames[1],sep = '') )
      #since geographic info for all files are the same (assuming we are working with the same data product)
      ###evaluate these values one time!
      ###getting the y values (longitudes in degrees east)
      nc.long.TRMM<-ncdf4::ncvar_get(nc,nc$dim[[1]])
      ####getting the x values (latitudes in degrees north)
      nc.lat.TRMM<-ncdf4::ncvar_get(nc,nc$dim[[2]])
      #gettig the climate data
      data<-ncdf4::ncvar_get(nc,myvarTRMM)
      #reorder the rows
      data<-data[ nrow(data):1, ]
      ncdf4::nc_close(nc)
      ###save the daily climate data values in a raster
      TRMM<-raster::raster(x=as.matrix(data),xmn=nc.long.TRMM[1],xmx=nc.long.TRMM[NROW(nc.long.TRMM)],ymn=nc.lat.TRMM[1],ymx=nc.lat.TRMM[NROW(nc.lat.TRMM)],crs=sp::CRS('+proj=longlat +datum=WGS84'))
      #obtain cell numbers within the TRMM raster
      cell.no<-raster::cellFromPolygon(TRMM, polys)
      #obtain lat/long values corresponding to watershed cells
      cell.longlat<-raster::xyFromCell(TRMM,unlist(cell.no))
      cell.rowCol <- raster::rowColFromCell(TRMM,unlist(cell.no))
      cell.values<-as.vector(TRMM)[unlist(cell.no)]
      study_area_records_TRMM<-data.frame(TRMM_ID=unlist(cell.no),cell.longlat,cell.rowCol)
      sp::coordinates (study_area_records_TRMM)<- ~x+y
      rm(data,TRMM)
      unlink(x='./temp', recursive = TRUE)
    }


    # creating a similarity table that connects IMERG and TRMM grids
    # calculate euclidean distances to know how to connect TRMM grids with IMERG grids
    for (i in 1 : nrow(study_area_records_IMERG))
    {
      distVec <- sp::spDistsN1(study_area_records_TRMM,study_area_records_IMERG[i,])
      minDistVec[[i]] <- min(distVec)
      closestSiteVec[[i]] <- which.min(distVec)
    }

    PointAssignIDs <- methods::as(study_area_records_TRMM[unlist(closestSiteVec),]$TRMM_ID,'numeric')
    PointsAssignCol <- methods::as(study_area_records_TRMM[unlist(closestSiteVec),]$col,'numeric')
    PointsAssignRow <- methods::as(study_area_records_TRMM[unlist(closestSiteVec),]$row,'numeric')

    FinalTable = data.frame(sp::coordinates(study_area_records_IMERG),ID=study_area_records_IMERG$ID,row=study_area_records_IMERG$row,col=study_area_records_IMERG$col,Elevation=study_area_records_IMERG$Elevation,
                            CloseTRMMIndex=PointAssignIDs,Distance=unlist(minDistVec),TRMMCol=PointsAssignCol,TRMMRow=PointsAssignRow)

    #### Begin writing SWAT climate input tables
    #### Get the SWAT file names and then put the first record date
    for(jj in 1:dim(FinalTable)[1])
    {
      if(dir.exists(Dir)==FALSE){dir.create(Dir)}
      filenameSWAT[[jj]]<-paste(myvarTRMM,FinalTable$ID[jj],sep='')
      filenameSWAT_TXT[[jj]]<-paste(Dir,filenameSWAT[[jj]],'.txt',sep='')
      #write the data begining date once!
      write(x=format(time_period[1],'%Y%m%d'),file=filenameSWAT_TXT[[jj]])
    }


    #### Write out the SWAT grid information master table
    OutSWAT<-data.frame(ID=FinalTable$ID,NAME=unlist(filenameSWAT),LAT=FinalTable$y,LONG=FinalTable$x,ELEVATION=FinalTable$Elevation)
    utils::write.csv(OutSWAT,filenametableKEY,row.names = F,quote = F)




    #### Start doing the work!
    #### iterate over days to extract record at IMERG grids estabished in 'FinalTable'


    for(kk in 1:length(time_period))
    {
      mon  <- format(time_period[kk],format='%m')
      year <- format(time_period[kk],format='%Y')

      #Decide here whether to use TRMM or IMERG based on data availability
      #Begin with TRMM first which means days before 2014-March-12
      if (time_period[kk] < as.Date('2014-03-12'))
      {
        myurl = paste(paste(url.TRMM.input,year,mon,sep = '/'),'/',sep = '')
        r <- httr::GET(myurl)
        filenames <- httr::content(r, "text")
        filenames <- XML::readHTMLTable(XML::htmlParse(filenames))[[1]]#getting the daily files at each monthly URL
        filenames <- unique(stats::na.exclude(stringr::str_extract(as.character(filenames$Name),'3B42.+(.nc4)')))
        tt<-as.Date(stringr::str_extract(stringr::str_extract(filenames,"20.+.7"),'[0-9]{1,8}'),format='%Y%m%d')
        pp<-tt%in%time_period[kk]
        filenames<-filenames[pp]
        # Extract the ncdf files

        for(ll in 1:length(filenames))# Iterating over each daily data file
        {
          #if(<=as.Date(end))
          #{
          # Downloading the file
          if(dir.exists('./temp/')==FALSE)
          {dir.create('./temp/')}
          utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[ll],sep = ''),destfile = paste('./temp/',filenames[ll],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')
          # Reading the ncdf file
          nc<-ncdf4::nc_open( paste('./temp/',filenames[ll],sep = '') )
          data<-ncdf4::ncvar_get(nc,myvarTRMM)
          # Reorder the rows
          data<-data[ nrow(data):1, ]
          ncdf4::nc_close(nc)
          ### Save the daily climate data values in a raster
          TRMM<-raster::raster(x=as.matrix(data),xmn=nc.long.TRMM[1],xmx=nc.long.TRMM[NROW(nc.long.TRMM)],ymn=nc.lat.TRMM[1],ymx=nc.lat.TRMM[NROW(nc.lat.TRMM)],crs=sp::CRS('+proj=longlat +datum=WGS84'))
          ### Obtaining daily climate values at TRMM grids near the IMERG grids that has been defined and explained earlier
          cell.values<-as.vector(TRMM)[FinalTable$CloseTRMMIndex]
          cell.values[is.na(cell.values)] <- -99.0 #filling missing data
          ### Looping through the TRMM points and writing out the daily climate data in SWAT format
          for(jj in 1:dim(FinalTable)[1])
          {
            write(x=cell.values[jj],filenameSWAT_TXT[[jj]],append=T,ncolumns = 1)
          }
          #}
          unlink(x='./temp', recursive = TRUE)
        }
      }


      else ## Now for dates equal to or greater than 2014 March 12
      {
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
            if(dir.exists('./temp/')==FALSE)
            {dir.create('./temp/')}
            utils::download.file(quiet = T,method='curl',url=paste(myurl,filenames[ll],sep = ''),destfile = paste('./temp/',filenames[ll],sep = ''), mode = 'wb', extra = '-n -c ~/.urs_cookies -b ~/.urs_cookies -L')
            # Reading the ncdf file
            nc<-ncdf4::nc_open( paste('./temp/',filenames[ll],sep = '') )
            data<-ncdf4::ncvar_get(nc,myvarIMERG)
            # Reorder the rows
            data<-data[ nrow(data):1, ]
            ncdf4::nc_close(nc)
            ###save the daily climate data values in a raster
            IMERG<-raster::raster(x=as.matrix(data),xmn=nc.long.IMERG[1],xmx=nc.long.IMERG[NROW(nc.long.IMERG)],ymn=nc.lat.IMERG[1],ymx=nc.lat.IMERG[NROW(nc.lat.IMERG)],crs=sp::CRS('+proj=longlat +datum=WGS84'))

            #obtain daily climate values at cells bounded with the study watershed (extract values from a raster)
            cell.values<-as.vector(IMERG)[FinalTable$ID]
            cell.values[is.na(cell.values)] <- -99.0 #filling missing data

            #This will get me the mean precipiation averaged over the watershed (this is not a weighted mean)
            #v <- extract(IMERG, polys,weights=T,cellnumbers=T,df=T,normalizeWeights=T,sp=T,fun='mean')

            #loop through the grid points to write out the daily climate data in a SWAT format
            for(jj in 1:dim(FinalTable)[1])
            {
              write(x=cell.values[jj],filenameSWAT_TXT[[jj]],append=T,ncolumns = 1)
            }

          }
          unlink(x='./temp', recursive = TRUE)
        }
      }






    }

  }
        else
          {
    cat('Sorry',paste(format(as.Date(start),'%b'),format(as.Date(start),'%Y'),sep=','),'is out of coverage for TRMM or IMERG data products.','  \n')
    cat('Please pick start date equal to or greater than 2000-Mar-01 to access TRMM and IMERG data products.','  \n')
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
