###7/4/22
#' Generate rainfall or air temperature as well as climate input stations file from NASA NEX-GDDP remote sensing climate change data products needed to drive various hydrological models.
#'
#' This function downloads climate change data of rainfall and air temperature from \acronym{NASA} Earth Exchange Global Daily Downscaled Projections \acronym{NEX-GDDP} \acronym{GSFC} servers, extracts data from grids within a specified watershed shapefile, and then generates tables in a format that any hydrological model requires for rainfall or air temperature data input. The function also generates the climate stations file input (file with columns: ID, File NAME, LAT, LONG, and ELEVATION) for those selected climatological grids that fall within the specified watershed. The \acronym{NASA} Earth Exchange Global Daily Downscaled Projections \acronym{NEX-GDDP} dataset is comprised of downscaled climate scenarios for the globe that are derived from the General Circulation Model \acronym{GCM} runs conducted under the Coupled Model Intercomparison Project Phase 5 \acronym{CMIP5} and across two of the four greenhouse gas emissions scenarios known as Representative Concentration Pathways \acronym{RCPs} (rcp45, rcp85).
#' @param Dir A directory name to store gridded rainfall and rain stations files.
#' @param watershed A study watershed shapefile spatially describing polygon(s) in a geographic projection sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs').
#' @param DEM A study watershed digital elevation model raster in a geographic projection sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs').
#' @param start Beginning date for gridded rainfall data.
#' @param end Ending date for gridded rainfall data.
#' @param model A climate modeling center and name from the the World Climate Research Programme \acronym{WCRP} global climate projections through the Coupled Model Intercomparison Project 5 \acronym{CMIP5} (e.g., \acronym{IPSL-CM5A-MR} which is Institut Pierre-Simon Laplace \acronym{CM5A-MR} model).
#' @param type  A flux data type. It's value can be \acronym{'pr'} for precipitation or \acronym{'tas'} for air temperature.
#' @param slice A scenario from the Representative Concentration Pathways. It's value can be \acronym{'rcp45'} , \acronym{'rcp85'}, or \acronym{'historical'}.
#'
#' @details A user should visit \url{https://disc.gsfc.nasa.gov/data-access} to register with the Earth Observing System Data and Information System (\acronym{NASA Earthdata}) and then authorize \acronym{NASA} \acronym{GESDISC} Data Access to successfully work with this function. The function accesses \acronym{NASA} Goddard Space Flight Center server for \acronym{IMERG} remote sensing data products at (\url{https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/}), and \acronym{NASA} Goddard Space Flight Center server for \acronym{NEX-GDDP} climate change data products at (\url{https://www.nccs.nasa.gov/services/climate-data-services}).  The function uses variable name ('pr') for rainfall in \acronym{NEX-GDDP} data products and variable name ('tas') for \acronym{NEX-GDDP} minimum ('tasmin') and maximum ('tasmax') air temperature data products. The \command{NEX-GDDP} function outputs gridded rainfall data in 'mm' and gridded air temperature (maximum and minimum) data in degrees 'C'.
#'
#' \acronym{NEX-GDDP} dataset is comprised of downscaled climate scenarios for the globe that are derived from the General Circulation Model \acronym{GCM} runs conducted under the Coupled Model Intercomparison Project Phase 5 \acronym{CMIP5} (Taylor et al. 2012) and across two of the four greenhouse gas emissions scenarios known as Representative Concentration Pathways \acronym{RCPs} (Meinshausen et al. 2011). The \acronym{CMIP5} \acronym{GCM} runs were developed in support of the Fifth Assessment Report of the Intergovernmental Panel on Climate Change \acronym{IPCC AR5}. This dataset includes downscaled projections from the 21 models and scenarios for which daily scenarios were produced and distributed under \acronym{CMIP5}.
#' The Bias-Correction Spatial Disaggregation \acronym{BCSD} method used in generating the \acronym{NEX-GDDP} dataset is a statistical downscaling algorithm specifically developed to address the current limitations of the global \acronym{GCM} outputs (Wood et al. 2002; Wood et al. 2004; Maurer et al. 2008; Thrasher et al. 2012).  The \acronym{NEX-GDDP} climate projections is downscaled at a spatial resolution of 0.25 degrees x 0.25 degrees (approximately 25 km x 25 km). The \command{NEX_GDDP_CMIP5} downscales the \acronym{NEX-GDDP} data to grid points of 0.1 degrees x 0.1 degrees following nearest point methods described by Mohammed et al. (2018).
#'
#' The \command{NEX_GDDP_CMIP5} function relies on 'curl' tool to transfer data from \acronym{NASA} servers to a user machine, using HTTPS supported protocol.  The 'curl' command embedded in this function to fetch precipitation/air temperature \acronym{NEX-GDDP}/ netcdf annual global files is designed to work seamlessly by appending appropriate logging information to the ".netrc" file and the cookies file ".urs_cookies". The ".netrc" and ".urs_cookies" files need to be stored at local directory before running any function in this package. Instructions on creating the ".netrc" and ".urs_cookies" files can be accessed at \url{https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget}. It is imperative to say here that a user machine should have 'curl' installed as a prerequisite to run \command{NEX_GDDP_CMIP5} or any other function part of the this package (\acronym{NASAaccess}).
#' @note
#' \code{start} should be equal to or greater than 2006-Jan-01 for \acronym{'rcp45'} or \acronym{'rcp85'} \acronym{RCP} climate scenario.
#'
#' \code{start} should be equal to or greater than 1950-Jan-01 and \code{end} should be equal to or less than 2005-Dec-31 for the \acronym{'historical'} \acronym{GCM} retrospective climate data.
#' @author Ibrahim Mohammed, \email{ibrahim.mohammed@@nasa.gov}
#'
#' @keywords NASA NEX-GDDP Climate Change CMIP5
#' @return A table that includes points ID, Point file name, Lat, Long, and Elevation information, and
#' a scalar of climate change gridded data values at each point within the study watershed in ascii format stored at \code{Dir}.
#' @references Maurer, E. P. and Hidalgo, H. G., 2008: Utility of daily vs. monthly large-scale climate data: an intercomparison of two statistical downscaling methods. Hydrology and Earth System Sciences, 12, 551-563, doi:10.5194/hess-12-551-2008.
#' @references Meinshausen, M. S.J. Smith, K. Calvin, J.S. Daniel, M.L.T. Kainuma, and et al., 2011: The RCP greenhouse gas concentrations and their extensions from 1765 to 2300. Climatic Change, 109, 213-241, doi:10.1007/s10584-011-0156-z.
#' @references Mohammed, I.N., J. Bolten, R. Srinivasan, and V. Lakshmi, 2018: Improved Hydrological Decision Support System for the Lower Mekong River Basin Using Satellite-Based Earth Observations. Remote Sensing, 10, 885, doi:10.3390/rs10060885.
#' @references Taylor, Karl E., Ronald J. Stouffer, Gerald A. Meehl, 2012: An Overview of CMIP5 and the Experiment Design. Bull. Amer. Meteor. Soc., 93, 485–498, doi:10.1175/BAMS-D-11-00094.1.
#' @references Thrasher, B., Maurer, E. P., McKellar, C., & Duffy, P. B., 2012: Technical Note: Bias correcting climate model simulated daily temperature extremes with quantile mapping. Hydrology and Earth System Sciences, 16(9), 3309-3314, doi:10.5194/hess-16-3309-2012
#' @references Wood, A.W., E.P. Maurer, A. Kumar, and D.P. Lettenmaier, 2002: Long-range experimental hydrologic forecasting for the eastern United States. J. Geophysical Research-Atmospheres, 107, 4429, doi:10.1029/2001JD000659.
#' @references Wood, A.W., L.R. Leung, V. Sridhar, and D.P. Lettenmaier, 2004: Hydrologic implications of dynamical and statistical approaches to downscaling climate model outputs. Climatic Change, 15,189-216, doi: 10.1023/B:CLIM.0000013685.99609.9e
#'
#' @examples
#' #Lower Mekong basin example
#' \dontrun{NEX_GDDP_CMIP5(Dir = "./INPUT/", watershed = "LowerMekong.shp",
#' DEM = "LowerMekong_dem.tif", start = "2060-12-1", end = "2060-12-3",
#' model = 'IPSL-CM5A-MR', type = 'pr', slice = 'rcp85')}
#' @import ncdf4 shapefiles rgeos maptools httr stringr rgdal XML utils sp methods
#' @importFrom stats na.exclude
#' @importFrom raster raster cellFromPolygon xyFromCell rowColFromCell extract
#' @export


NEX_GDDP_CMIP5=function(Dir='./INPUT/', watershed ='LowerMekong.shp', DEM = 'LowerMekong_dem.tif', start = '2060-12-1', end = '2060-12-3', model = 'IPSL-CM5A-MR', type = 'pr',slice = 'rcp85')
{

  #if there is no logging information then update the netrc file with NEX-GDDP info
  if(file.exists('~/.netrc')==TRUE||file.exists('~/_netrc')==TRUE)
  {
    url.IMERG.input <- 'https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/'
    url.GDDP.input <- 'https://portal.nccs.nasa.gov/datashare/NEXGDDP/BCSD/'
    myvarIMERG <- 'precipitationCal'
    myvarNAME <- 'climate'
    #check the DATASHARE NEX-GDDP availability
    if(httr::status_code(GET(url.GDDP.input))==200)
    {
      
      if(type=='pr'){ftp <- paste(url.GDDP.input,slice,'/','day','/','atmos','/',type,'/','r1i1p1','/','v1.0','/',sep='')}
      if(type=='tas'){ftp_min <- paste(url.GDDP.input,slice,'/','day','/','atmos','/',type,'min','/','r1i1p1','/','v1.0','/',sep='');ftp_max <- paste(url.GDDP.input,slice,'/','day','/','atmos','/',type,'max','/','r1i1p1','/','v1.0','/',sep='')}
      ####Before getting to work on this function do this check on start and end dates
      if (as.Date(start) >= as.Date('1950-01-01') &  as.Date(end) <= as.Date('2100-12-31') & slice == 'rcp85' | slice == 'rcp45' | slice == 'historical')
      {
        
        # Constructing time series based on start and end input days!
        time_period <- seq.Date(from = as.Date(start), to = as.Date(end), by = 'day')
        # Reading cell elevation data (DEM should be in geographic projection)
        watershed.elevation <- raster::raster(DEM)
        # Reading the study Watershed shapefile
        polys <- rgdal::readOGR(dsn=watershed,verbose = F)
        # To address missing parameters in projection strings
        polys <- sp::spTransform(polys,CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
        # Grid climate master file name
        filenametableKEY<-paste(Dir,type, 'Grid_Master.txt',sep='')
        # Creating empty lists
        filenameSWAT     <- list()
        filenameSWAT_TXT <- list()
        closestSiteVec   <- list()
        minDistVec       <- list()
        cell.temp.values <- list()
        # The IMERG data grid information
        # Read a dummy day to extract spatial information and assign elevation data to the grids within the study watersheds
        DUMMY_DATE <- as.Date('2014-09-01')
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
          test1<-file.info(paste('./temp/',filenames[1],sep= ''))$size
          stopifnot('The GPM IMERG server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test1 > 3.5e6)
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
          IMERG<-raster::raster(x=as.matrix(data),xmn=nc.long.IMERG[1],xmx=nc.long.IMERG[NROW(nc.long.IMERG)],ymn=nc.lat.IMERG[1],ymx=nc.lat.IMERG[NROW(nc.lat.IMERG)],crs=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
          #obtain cell numbers within the IMERG raster
          #cell.no<-raster::cellFromPolygon(IMERG, polys)
          suppressWarnings(cell.no<-raster::cellFromPolygon(IMERG, polys))
          #obtain lat/long values corresponding to watershed cells
          cell.longlat<-raster::xyFromCell(IMERG,unlist(cell.no))
          cell.rowCol <- raster::rowColFromCell(IMERG,unlist(cell.no))
          points_elevation<-raster::extract(x=watershed.elevation,y=cell.longlat,method='simple')
          study_area_records_IMERG<-data.frame(ID=unlist(cell.no),cell.longlat,cell.rowCol,Elevation=points_elevation)
          sp::coordinates (study_area_records_IMERG)<- ~x+y
          rm(data,IMERG)
          # The NEX-GDDP data grid information
          # Use the same dummy date defined above since NEX-GDDP has data from 1950 to 2100.
          # Using dummy date and file info for a file in the NEX-GDDP dataset
          # downloading one file
          if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
          type.start<-ifelse(isTRUE(type=="pr")==TRUE,type,paste(type,'min',sep=''))
          
          
          filename.start <- paste(paste(type.start,'day','BCSD',slice,'r1i1p1',model,format(time_period[1],"%Y"),sep = '_'),'.nc',sep="")
          myurl <- paste(ifelse(isTRUE(type=="pr")==TRUE,ftp,ftp_min),filename.start,sep = '')
          utils::download.file(quiet = T, method = 'curl', url = myurl, destfile = paste('./temp/',filename.start,sep= ''), mode = 'wb', extra = '-L')
        
          test2<-file.info(paste('./temp/',filename.start,sep= ''))$size
          stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test2 > 7.0e6)
          #reading ncdf file
          nc<-ncdf4::nc_open( paste('./temp/',filename.start,sep= '') )
          #since geographic info for all NEX files are the same
          ###evaluate these values at one time!
          ###getting the x values (longitudes in degrees east, 0 to +360) so it needed to be converted to -180 to 180)
          nc.long.NEXGDDP<-ncdf4::ncvar_get(nc,nc$dim[[3]])
          ####getting the y values (latitudes in degrees north, -90 to +90)
          nc.lat.NEXGDDP<-ncdf4::ncvar_get(nc,nc$dim[[2]])
          #getting the climate data for one data as a dummy matrix
          catch <- ifelse(isTRUE(type=="pr")==TRUE,type,paste(type,'min',sep=""))
          data<-ncdf4::ncvar_get(nc,catch, start = c(1,1,1) , count = c(-1, -1 ,1))
          #transpose the data
          data <- raster::t(data)
          #reorder the rows
          data<-data[ nrow(data):1, ]
          ncdf4::nc_close(nc)
          ###save the daily climate data values in a raster
          NEX<-raster::raster(x=as.matrix(data),xmn=nc.long.NEXGDDP[1],xmx=nc.long.NEXGDDP[NROW(nc.long.NEXGDDP)],ymn=nc.lat.NEXGDDP[1],ymx=nc.lat.NEXGDDP[NROW(nc.lat.NEXGDDP)],crs=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
          ###rotate the raster to obtain the longitudes extent -180 to 180
          NEX<-raster::rotate(NEX)
          #obtain cell numbers within the NEX-GDDP raster
          #cell.no<-raster::cellFromPolygon(NEX, polys)
          suppressWarnings(cell.no<-raster::cellFromPolygon(NEX, polys))
          ##check cell.no to address small watershed
          if(length(unlist(cell.no))==0)
          {
            #cell.no<-raster::cellFromPolygon(NEX, polys, weights = TRUE)[[1]][,"cell"][1]
            suppressWarnings(cell.no<-raster::cellFromPolygon(NEX, polys, weights = TRUE)[[1]][,"cell"][1])
          }
          #obtain lat/long values corresponding to watershed cells
          cell.longlat<-raster::xyFromCell(NEX,unlist(cell.no))
          cell.rowCol <- raster::rowColFromCell(NEX,unlist(cell.no))
          study_area_records_NEX<-data.frame(NEX_ID=unlist(cell.no),cell.longlat,cell.rowCol)
          sp::coordinates (study_area_records_NEX)<- ~x+y
          rm(data,NEX)
          unlink(x='./temp', recursive = TRUE)
        }
        # creating a similarity table that connects IMERG and NEX-GDDP grids
        # calculate euclidean distances to know how to connect NEX-GDDP grids with IMERG grids
        for (i in 1 : nrow(study_area_records_IMERG))
        {
          distVec <- sp::spDistsN1(study_area_records_NEX,study_area_records_IMERG[i,])
          minDistVec[[i]] <- min(distVec)
          closestSiteVec[[i]] <- which.min(distVec)
        }
        
        PointAssignIDs <- methods::as(study_area_records_NEX[unlist(closestSiteVec),]$NEX_ID,'numeric')
        PointsAssignCol <- methods::as(study_area_records_NEX[unlist(closestSiteVec),]$col,'numeric')
        PointsAssignRow <- methods::as(study_area_records_NEX[unlist(closestSiteVec),]$row,'numeric')
        FinalTable = data.frame(sp::coordinates(study_area_records_IMERG),ID=study_area_records_IMERG$ID,row=study_area_records_IMERG$row,col=study_area_records_IMERG$col,Elevation=study_area_records_IMERG$Elevation,
                                CloseNEXIndex=PointAssignIDs,Distance=unlist(minDistVec),NEXCol=PointsAssignCol,NEXRow=PointsAssignRow)
        #### Begin writing climate input tables
        #### Get the climate file names and then put the first record date
        for(jj in 1:dim(FinalTable)[1])
        {
          if(dir.exists(Dir)==FALSE){dir.create(Dir,recursive = TRUE)}
          filenameSWAT[[jj]]<-paste(type, myvarNAME,FinalTable$ID[jj],sep='')
          filenameSWAT_TXT[[jj]]<-paste(Dir,filenameSWAT[[jj]],'.txt',sep='')
          #write the data beginning date once!
          write(x=format(time_period[1],'%Y%m%d'),file=filenameSWAT_TXT[[jj]])
        }
        #### Write out the climate grid information master table
        Out<-data.frame(ID=FinalTable$ID,NAME=unlist(filenameSWAT),LAT=FinalTable$y,LONG=FinalTable$x,ELEVATION=FinalTable$Elevation)
        utils::write.csv(Out,filenametableKEY,row.names = F,quote = F)
        #### Start doing the work!
        #### iterate over days to extract record from NEX-GDDP at IMERG grid locations established in the 'FinalTable' dataframe
        if(type == 'pr')
        {
          for(kk in 1:length(time_period))
          {
            timestart <- time_period[kk]
            if(format(timestart,"%m-%d") != "02-29")
            {
              #Here I want to have a Julian date for a fixed non-leap year. The NEX-GDPP data has 365 days for every year! 
              dayjuilan <- as.numeric(format(as.Date(paste('2063',format(timestart,'%m'),format(timestart,'%d'),sep="-")),"%j"))
              filename <- paste(type,'_day_BCSD_',slice,'_r1i1p1_',model,'_',format(timestart,"%Y"),'.nc',sep = '')
              myurl <- paste(ftp,filename,sep = '')
              # downloading file
              if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
              if(file.exists(paste('./temp/',filename,sep= ''))==FALSE){utils::download.file(quiet = T, method = 'curl', url = myurl, destfile = paste('./temp/',filename,sep= ''), mode = 'wb', extra = '-L')}
              # Reading the ncdf file
              test3<-file.info(paste('./temp/',filename,sep= ''))$size
              stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test3 > 6.0e6)
              nc <- ncdf4::nc_open( paste('./temp/',filename,sep = '') )
              data <- ncdf4::ncvar_get(nc,type, start = c(1,1,dayjuilan) , count = c(-1, -1 ,1))
              #transpose the data
              data <- raster::t(data)
              #reorder the rows
              data<-data[ nrow(data):1, ]
              ncdf4::nc_close(nc)
              ###save the daily climate data values in a raster
              NEX<-raster::raster(x=as.matrix(data),xmn=nc.long.NEXGDDP[1],xmx=nc.long.NEXGDDP[NROW(nc.long.NEXGDDP)],ymn=nc.lat.NEXGDDP[1],ymx=nc.lat.NEXGDDP[NROW(nc.lat.NEXGDDP)],crs=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
              ###rotate to obtain the longitudes in -180 to 180
              NEX<-raster::rotate(NEX)
              ### Obtaining daily climate values at NEX grids near the IMERG grids that has been defined and explained earlier, convert units from kg m^-2 s^-1 to mm day^-1 by multiplying with 86400 (60*60*24)
              cell.values<-as.vector(NEX)[FinalTable$CloseNEXIndex]*86400
              cell.values[is.na(cell.values)] <- '-99.0' #filling missing data
            }
            
            else
            {
              ###the date is Feb 29th and NEX-GDDP has no value for it
              cell.values <- rep('-99.0',dim(FinalTable)[1])
              
            }
            
            ### Looping through the NEX points and writing out the daily climate data
            for(jj in 1:dim(FinalTable)[1])
            {
              write(x=cell.values[jj],filenameSWAT_TXT[[jj]],append=T,ncolumns = 1)
            }
            
            #empty memory and getting ready for the next day!
            cell.values<-list()
          }
        }
        else
        {
          for(jj in 1:length(time_period))
          {
            timestart <- time_period[jj]
            if(format(timestart,"%m-%d") != "02-29")
            {
              #Here I want to have a Julian date for a fixed non-leap year. The NEX-GDPP data has 365 days for every year! 
              dayjuilan <- as.numeric(format(as.Date(paste('2063',format(timestart,'%m'),format(timestart,'%d'),sep="-")),"%j"))
              typemin <- paste(type,'min',sep='')
              typemax <- paste(type,'max',sep='')
              filename_min <- paste(typemin,'_day_BCSD_',slice,'_r1i1p1_',model,'_',format(timestart,"%Y"),'.nc',sep = '')
              filename_max <- paste(typemax,'_day_BCSD_',slice,'_r1i1p1_',model,'_',format(timestart,"%Y"),'.nc',sep = '')
              myurl_min <- paste(ftp_min,filename_min,sep = '')
              myurl_max <- paste(ftp_max,filename_max,sep = '')
              # downloading file
              if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
              if(file.exists(paste('./temp/',filename_min,sep= ''))==FALSE|file.exists(paste('./temp/',filename_max,sep= ''))==FALSE){utils::download.file(quiet = T, method = 'curl', url = myurl_min, destfile = paste('./temp/',filename_min,sep= ''), mode = 'wb', extra = '-L');utils::download.file(quiet = T, method = 'curl', url = myurl_max, destfile = paste('./temp/',filename_max,sep= ''), mode = 'wb', extra = '-L')}
              # Reading the tasmin ncdf file
              test4<-file.info(paste('./temp/',filename_min,sep= ''))$size
              stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test4 > 6.0e6)
              nc_min <- ncdf4::nc_open( paste('./temp/',filename_min,sep = '') )
              data_min <- ncdf4::ncvar_get(nc_min,typemin, start = c(1,1,dayjuilan) , count = c(-1, -1 ,1))
              #transpose the data
              data_min <- raster::t(data_min)
              #reorder the rows
              data_min<-data_min[ nrow(data_min):1, ]
              ncdf4::nc_close(nc_min)
              # Reading the tmax ncdf file
              test5<-file.info(paste('./temp/',filename_max,sep= ''))$size
              stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test5 > 6.0e6)
              nc_max <- ncdf4::nc_open( paste('./temp/',filename_max,sep = '') )
              data_max <- ncdf4::ncvar_get(nc_max,typemax, start = c(1,1,dayjuilan) , count = c(-1, -1 ,1))
              #transpose the data
              data_max <- raster::t(data_max)
              #reorder the rows
              data_max<-data_max[ nrow(data_max):1, ]
              ncdf4::nc_close(nc_max)
              ###save the daily climate data values in a raster
              NEX_min<-raster::raster(x=as.matrix(data_min),xmn=nc.long.NEXGDDP[1],xmx=nc.long.NEXGDDP[NROW(nc.long.NEXGDDP)],ymn=nc.lat.NEXGDDP[1],ymx=nc.lat.NEXGDDP[NROW(nc.lat.NEXGDDP)],crs=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
              NEX_max<-raster::raster(x=as.matrix(data_max),xmn=nc.long.NEXGDDP[1],xmx=nc.long.NEXGDDP[NROW(nc.long.NEXGDDP)],ymn=nc.lat.NEXGDDP[1],ymx=nc.lat.NEXGDDP[NROW(nc.lat.NEXGDDP)],crs=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
              ###rotate to obtain the longitudes in -180 to 180
              NEX_min<-raster::rotate(NEX_min)
              NEX_max<-raster::rotate(NEX_max)
              ### Obtaining daily climate values at NEX grids near the IMERG grids that has been defined and explained earlier, convert units to C by substracting 273.16
              cell.values_min<-as.vector(NEX_min)[FinalTable$CloseNEXIndex] - 273.16 #convert to degree C
              cell.values_max<-as.vector(NEX_max)[FinalTable$CloseNEXIndex] - 273.16 #convert to degree C
              cell.values_min[is.na(cell.values_min)] <- '-99.0' #filling missing data
              cell.values_max[is.na(cell.values_max)] <- '-99.0' #filling missing data
            }
            else
            {
              ###the date is Feb 29th and NEX-GDDP has no value for it
              cell.values_min <- rep('-99.0', dim(FinalTable)[1]) #filling missing data
              cell.values_max <- rep('-99.0', dim(FinalTable)[1]) #filling missing data
              
            }
            ### Looping through the NEX points and writing out the daily climate data
            for(k in 1:dim(FinalTable)[1])
            {
              cell.temp.values[[k]]<-paste(cell.values_max[k],cell.values_min[k],sep=',')
              write(x=cell.temp.values[[k]],filenameSWAT_TXT[[k]],append=T,ncolumns = 1)
            }
            #empty memory and getting ready for the next day!
            cell.temp.values<-list()
          }
          
        }
        
        
        unlink(x='./temp', recursive = TRUE)
      }
      
      else
      {
        cat('Sorry!',paste(format(as.Date(start),'%b'),format(as.Date(start),'%Y'),sep=','),'is out of coverage for the NEX-GDDP data products.','  \n')
        cat('Please pick start and end dates following notes described at the function notes to access the NEX-GDDP data products.','  \n')
        cat('Thank you!','  \n')
      }
    }
    
      else
      {
        cat('Sorry!','  \n')
        cat('The NEX-GDDP DATASHARE server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.','  \n')
        cat('Thank you.','  \n')
      }
    


  }

  else
  {
    cat('Sorry!','  \n')
    cat('You need to create one/two file(s) named ".netrc" , "_netrc" and ".urs_cookies" at your home Directory. The "_netrc" file only needed for Windows users.','  \n')
    cat('Instructions on creating the ".netrc" and the ".urs_cookies" files can be accessed at https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget','  \n')
    cat('For Windows users follow instructions on creating the "_netrc" file at https://github.com/imohamme/NASAaccess/wiki/Curl-installation-on-Windows','  \n')
    cat('Thank you.','  \n')
  }

}
