###11/11/22
#' Generate rainfall or air temperature as well as climate input stations file from NASA NEX-GDDP-CMIP6 remote sensing climate change data products needed to drive various hydrological models.
#'
#' This function downloads climate change data of rainfall and air temperature from \acronym{NASA} Earth Exchange Global Daily Downscaled Projections \acronym{NEX-GDDP-CMIP6} \acronym{AMES} servers, extracts data from grids within a specified watershed shapefile, and then generates tables in a format that any hydrological model requires for rainfall or air temperature data input. The function also generates the climate stations file input (file with columns: ID, File NAME, LAT, LONG, and ELEVATION) for those selected climatological grids that fall within the specified watershed. The \acronym{NASA} Earth Exchange Global Daily Downscaled Projections \acronym{NEX-GDDP-CMIP6} data set is comprised of downscaled climate scenarios for the globe that are derived from the General Circulation Model \acronym{GCM} runs conducted under the Coupled Model Intercomparison Project Phase 6 \acronym{CMIP6} and across two of the four "Tier 1" greenhouse gas emissions scenarios known as Shared Socioeconomic Pathways \acronym{SSPs}.
#' @param Dir A directory name to store gridded rainfall and rain stations files.
#' @param watershed A study watershed shapefile spatially describing polygon(s) in a geographic projection sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs').
#' @param DEM A study watershed digital elevation model raster in a geographic projection sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs').
#' @param start Beginning date for gridded rainfall data.
#' @param end Ending date for gridded rainfall data.
#' @param model A climate modeling center and name from the the World Climate Research Programme \acronym{WCRP} global climate projections through the Coupled Model Intercomparison Project 6 \acronym{CMIP6} (e.g., \acronym{MIROC6} which is the sixth version of the Model for Interdisciplinary Research on Climate \acronym{MIROC} model).
#' @param type  A flux data type. It's value can be \acronym{'pr'} for precipitation or \acronym{'tas'} for air temperature.
#' @param slice A scenario from the Shared Socioeconomic Pathways (SSPs). It's value can be \acronym{'ssp126'}, \acronym{'ssp245'}, \acronym{'ssp370'}, \acronym{'ssp585'}, or \acronym{'historical'}.
#'
#' @details A user should visit \url{https://disc.gsfc.nasa.gov/data-access} to register with the Earth Observing System Data and Information System (\acronym{NASA Earthdata}) and then authorize \acronym{NASA} \acronym{GESDISC} Data Access to successfully work with this function.
#' The function accesses \acronym{NASA} Goddard Space Flight Center server for \acronym{IMERG} remote sensing data products at (\url{https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/}), and \acronym{NASA} AMES Research Center server for \acronym{NEX-GDDP-CMIP6}
#' climate change data products at \url{https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6}.  The function uses variable name ('pr') for rainfall in \acronym{NEX-GDDP-CMIP6} data products and variable name ('tas') for \acronym{NEX-GDDP-CMIP6} minimum ('tasmin') and maximum ('tasmax')
#' air temperature data products. The \command{NEX_GDDP_CMIP6} function outputs gridded rainfall data in 'mm' and gridded air temperature (maximum and minimum) data in degrees 'C'.
#'
#' \acronym{NEX-GDDP-CMIP6} dataset is comprised of downscaled climate scenarios for the globe that are derived from the General Circulation Model \acronym{GCM} runs conducted under the Coupled Model Intercomparison Project Phase 6 \acronym{CMIP6} (Eyring et al. 2016)
#' and across the four "Tier 1" greenhouse gas emissions scenarios known as Shared Socioeconomic Pathways \acronym{SSPs} (O'Neil et al. 2016; Meinshausen et al. 2020). The \acronym{CMIP6} \acronym{GCM} runs were developed in support of the Sixth Assessment Report
#' of the Intergovernmental Panel on Climate Change \acronym{IPCC AR6}. This data set includes downscaled projections from the 35 models and scenarios for which daily scenarios were produced and distributed under \acronym{CMIP6}. Please visit \acronym{NCCS} Dataportal - Datashare \url{https://ds.nccs.nasa.gov/thredds2/catalog/AMES/NEX/GDDP-CMIP6/catalog.html} to ensure that the requested model and greenhouse gas emissions scenario (\acronym{SSPs}) is available on the server before you execute your run.
#' The Bias-Correction Spatial Disaggregation \acronym{BCSD} method used in generating the \acronym{NEX-GDDP-CMIP6} data set is a statistical downscaling algorithm specifically developed to address the current limitations of the global \acronym{GCM} outputs
#' (Wood et al. 2002; Wood et al. 2004; Maurer et al. 2008; Thrasher et al. 2012).  The \acronym{NEX-GDDP-CMIP6} climate projections is downscaled at a spatial resolution of 0.25 degrees x 0.25 degrees (approximately 25 km x 25 km).
#' The \command{NEX_GDDP_CMIP6} downscales the \acronym{NEX-GDDP-CMIP6} data to grid points of 0.1 degrees x 0.1 degrees following nearest point methods described by Mohammed et al. (2018).
#'
#' The \command{NEX_GDDP_CMIP6} function relies on 'curl' tool to transfer data from \acronym{NASA} servers to a user machine, using HTTPS supported protocol.  The 'curl' command embedded in this function to fetch precipitation/air temperature \acronym{NEX-GDDP-CMIP6}/ netcdf annual global files is designed to work seamlessly by appending appropriate logging information to the ".netrc" file and the cookies file ".urs_cookies". The ".netrc" and ".urs_cookies" files need to be stored at local directory before running any function in this package. Instructions on creating the ".netrc" and ".urs_cookies" files can be accessed at \url{https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget}. It is imperative to say here that a user machine should have 'curl' installed as a prerequisite to run \command{NEX_GDDP_CMIP6} or any other function part of the this package (\acronym{NASAaccess}).
#' @note
#' \code{start} should be equal to or greater than 2015-Jan-01 for \acronym{'ssp126'}, \acronym{'ssp245'}, \acronym{'ssp370'}, or \acronym{'ssp585'} \acronym{SSPs} climate scenario.
#'
#' \code{start} should be equal to or greater than 1950-Jan-01 and \code{end} should be equal to or less than 2014-Dec-31 for the \acronym{'historical'} \acronym{GCM} retrospective climate data.
#' @author Ibrahim Mohammed, \email{ibrahim.mohammed@@nasa.gov}
#'
#' @keywords NASA NEX-GDDP-CMIP6 Climate Change CMIP6
#' @return A table that includes points ID, Point file name, Lat, Long, and Elevation information formatted to be read with hydrological models and
#' a scalar of climate change gridded data values at each point within the study watershed in ascii format needed by hydrological model weather inputs will be stored at \code{Dir}.
#' @references Eyring, V., Bony, S., Meehl, G.A., Senior, C.A., Stevens, B., and et al., 2016. Overview of the Coupled Model Intercomparison Project Phase 6 (CMIP6) experimental design and organization. Geoscientific Model Development, 9, 1937-1958, doi: 10.5194/gmd-9-1937-2016
#' @references Maurer, E. P. and Hidalgo, H. G., 2008. Utility of daily vs. monthly large-scale climate data: an intercomparison of two statistical downscaling methods. Hydrology and Earth System Sciences, 12, 551-563, doi: 10.5194/hess-12-551-2008.
#' @references Meinshausen, M., Nicholls, Z.R.J., Lewis, J., Gidden, M.J., Vogel, E., and et al., 2020. The shared socio-economic pathway (SSP) greenhouse gas concentrations and their extensions to 2500. Geoscientific Model Development, 13, 3571-3605, doi: 10.5194/gmd-13-3571-2020
#' @references Meinshausen, M., Smith, S.J., Calvin, K., Daniel, J.S., Kainuma, M.L.T., and et al., 2011. The RCP greenhouse gas concentrations and their extensions from 1765 to 2300. Climatic Change, 109, 213-241, doi: 10.1007/s10584-011-0156-z.
#' @references Mohammed, I.N., Bolten, J., Srinivasan, R., and V. Lakshmi, 2018. Improved Hydrological Decision Support System for the Lower Mekong River Basin Using Satellite-Based Earth Observations. Remote Sensing, 10, 885, doi: 10.3390/rs10060885.
#' @references O'Neill, B.C., Tebaldi, C., van Vuuren, D.P., Eyring, V., Friedlingstein, and et al., 2016. The Scenario Model Intercomparison Project (ScenarioMIP) for CMIP6. Geoscientific Model Development, 9, 3461-3482, doi: 10.5194/gmd-9-3461-2016
#' @references Thrasher, B., Maurer, E. P., McKellar, C., and P. B. Duffy, 2012. Technical Note: Bias correcting climate model simulated daily temperature extremes with quantile mapping. Hydrology and Earth System Sciences, 16(9), 3309-3314, doi:1 0.5194/hess-16-3309-2012
#' @references Wood, A.W., E.P. Maurer, A. Kumar, and D.P. Lettenmaier, 2002: Long-range experimental hydrologic forecasting for the eastern United States. J. Geophysical Research-Atmospheres, 107, 4429, doi: 10.1029/2001JD000659.
#' @references Wood, A.W., L.R. Leung, V. Sridhar, and D.P. Lettenmaier, 2004: Hydrologic implications of dynamical and statistical approaches to downscaling climate model outputs. Climatic Change, 15,189-216, doi: 10.1023/B:CLIM.0000013685.99609.9e
#'
#' @examples
#' #Lower Mekong basin example
#' \dontrun{NEX_GDDP_CMIP6(Dir = "./INPUT/", watershed = "LowerMekong.shp",
#' DEM = "LowerMekong_dem.tif", start = "2060-12-1", end = "2060-12-3",
#' model = 'MIROC6', type = 'pr', slice = 'ssp245')}
#' @import ncdf4 shapefiles rgeos maptools httr stringr rgdal XML utils sp methods
#' @importFrom stats na.exclude
#' @importFrom raster raster cellFromPolygon xyFromCell rowColFromCell extract
#' @export


NEX_GDDP_CMIP6=function(Dir='./INPUT/', watershed ='LowerMekong.shp', DEM = 'LowerMekong_dem.tif', start = '2060-12-1', end = '2060-12-3', model = 'MIROC6', type = 'pr',slice = 'ssp245')
{

  #if there is no logging information then update the netrc file with NEX-GDDP info
  if(file.exists('~/.netrc')==TRUE||file.exists('~/_netrc')==TRUE)
  {

    url.IMERG.input <- 'https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDF.06/'
    url.GDDP.input <- 'https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/'
    url.catalog.input <- paste('https://ds.nccs.nasa.gov/thredds2/catalog/AMES/NEX/GDDP-CMIP6/',model,slice,'catalog.html',sep="/")
    myvarIMERG <- 'precipitationCal'
    myvarNAME <- 'climate'
    #check the DATASHARE NEX-GDDP availability
    if(httr::status_code(GET(url.catalog.input))==200)
    {
      #let's get the Variant (e.g., r1i1p1f1)
      rr <- readLines(url.catalog.input)
      dump.address <- "/thredds2/folder.gif" ; my.pattern <- "[[:alnum:]]{8}"
      folderlines <- grep(dump.address,rr,value=TRUE)
      Variant<- unique(as.vector(stringr::str_match_all(folderlines,my.pattern)[[2]]))[2]
      rm(dump.address,folderlines,my.pattern,rr)
      if(type=='pr'){ftp <- paste(url.GDDP.input,model,'/',slice,'/',Variant,'/',type,'/',type,'_','day','_',model,'_',slice,'_',Variant,'_','gn','_',sep='')}
      if(type=='tas'){ftp_min <- paste(url.GDDP.input,model,'/',slice,'/',Variant,'/',type,'min','/',type,'min','_','day','_',model,'_',slice,'_',Variant,'_','gn','_',sep='');ftp_max <- paste(url.GDDP.input,model,'/',slice,'/',Variant,'/',type,'max','/',type,'max','_','day','_',model,'_',slice,'_',Variant,'_','gn','_',sep='')}
      ####Before getting to work on this function do this check on start and end dates
       if (as.Date(start) >= as.Date('1950-01-01') &  as.Date(end) <= as.Date('2100-12-31') & slice == 'ssp126' | slice == 'ssp245' | slice == 'ssp370' | slice == 'ssp585' | slice == 'historical')
          {

      # Constructing time series based on start and end input days!
      time_period <- seq.Date(from = as.Date(start), to = as.Date(end), by = 'day')
      # Reading cell elevation data (DEM should be in geographic projection)
      watershed.elevation <- raster::raster(DEM)
      # Reading the study Watershed shapefile
      polys <- rgdal::readOGR(dsn=watershed,verbose = F)
      # To address missing parameters in projection strings
      polys <- sp::spTransform(polys,CRSobj = c('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
      # Hydrological model climate master file name
      filenametableKEY<-paste(Dir,type, 'Grid_Master.txt',sep='')
      # Creating empty lists
      filenameSWAT     <- list()
      filenameSWAT_TXT <- list()
      closestSiteVec   <- list()
      minDistVec       <- list()
      cell.temp.values <- list()
      # The IMERG data grid information
      # Read a dummy day to extract spatial information and assign elevation data to the grids within the study watersheds
      DUMMY_DATE <- as.Date('2016-09-01')
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
        # The NEX-GDDP-CMIP6 data grid information
        # Use the same dummy date defined above since NEX-GDDP-CMIP6 has data from 1950 to 2100.
        # Using dummy date and file info for a file in the NEX-GDDP-CMIP6 dataset
        # downloading one file
        if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
	      utils::download.file(quiet = T, method = 'curl', url = 'https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/ACCESS-CM2/ssp585/r1i1p1f1/tasmax/tasmax_day_ACCESS-CM2_ssp585_r1i1p1f1_gn_2015.nc?var=tasmax&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=2015-09-01T12%3A00%3A00Z&time_end=2015-09-02T12%3A00%3A00Z&timeStride=1', destfile = paste('./temp/','tasmax_day_ssp585_r1i1p1f1_ACCESS-CM2_2015.nc',sep= ''), mode = 'wb', extra = '-L')
        test2<-file.info(paste('./temp/','tasmax_day_ssp585_r1i1p1f1_ACCESS-CM2_2015.nc',sep= ''))$size
        stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test2 > 6.0e6)
        #reading ncdf file
        nc<-ncdf4::nc_open( paste('./temp/','tasmax_day_ssp585_r1i1p1f1_ACCESS-CM2_2015.nc',sep = '') )
        #since geographic info for all NEX files are the same
        ###evaluate these values at one time!
        ###getting the x values (longitudes in degrees east, 0 to +360) so it needed to be converted to -180 to 180)
        nc.long.NEXGDDP<-ncdf4::ncvar_get(nc,nc$dim[[3]])
        ####getting the y values (latitudes in degrees north, -90 to +90)
        nc.lat.NEXGDDP<-ncdf4::ncvar_get(nc,nc$dim[[2]])
        #getting the climate data
        data<-ncdf4::ncvar_get(nc,'tasmax', start = c(1,1,1) , count = c(-1, -1 ,1))
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

      }
      # creating a similarity table that connects IMERG and TRMM grids
      # calculate euclidean distances to know how to connect TRMM grids with IMERG grids
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
      #### Begin writing hydrological model climate input tables
      #### Get the hydrological model file names and then put the first record date
      for(jj in 1:dim(FinalTable)[1])
      {
        if(dir.exists(Dir)==FALSE){dir.create(Dir,recursive = TRUE)}
        filenameSWAT[[jj]]<-paste(type, myvarNAME,FinalTable$ID[jj],sep='')
        filenameSWAT_TXT[[jj]]<-paste(Dir,filenameSWAT[[jj]],'.txt',sep='')
        #write the data beginning date once!
        write(x=format(time_period[1],'%Y%m%d'),file=filenameSWAT_TXT[[jj]])
      }
      #### Write out the hydrological model grid information master table
      OutHydrology<-data.frame(ID=FinalTable$ID,NAME=unlist(filenameSWAT),LAT=FinalTable$y,LONG=FinalTable$x,ELEVATION=FinalTable$Elevation)
      utils::write.csv(OutHydrology,filenametableKEY,row.names = F,quote = F)
      #### Start doing the work!
      #### iterate over days to extract record from NEX-GDDP at IMERG grid locations established in the 'FinalTable' dataframe
      if(type == 'pr')
      {
        for(kk in 1:length(time_period))
        {
          timestart <- time_period[kk]
          timeend <- timestart + 1
          timeyear <- format(timestart,"%Y")
          filename <- paste(type,'_day_',slice,'_',Variant,'_',model,'_',as.character(timestart),'_',as.character(timeend),'.nc',sep = '')
          myurl <- paste(ftp,timeyear,'.nc?','var=',type,'&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=',as.character(timestart),'T12%3A00%3A00Z&time_duration=P1D','&timeStride=1',sep = '')

            # downloading file
            if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
            if(file.exists(paste('./temp/',filename,sep= ''))==FALSE){utils::download.file(quiet = T, method = 'curl', url = myurl, destfile = paste('./temp/',filename,sep= ''), mode = 'wb', extra = '-L')}
            # Reading the ncdf file
            test3<-file.info(paste('./temp/',filename,sep= ''))$size
            stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test3 > 3.0e6)
            nc <- ncdf4::nc_open( paste('./temp/',filename,sep = '') )
            data <- ncdf4::ncvar_get(nc,type, start = c(1,1,1) , count = c(-1, -1 ,1))
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
            ### Looping through the NEX points and writing out the daily climate data in hydrology model format
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
          timeend <- timestart + 1
          timeyear <- format(timestart,"%Y")
          typemin <- paste(type,'min',sep='')
          typemax <- paste(type,'max',sep='')
          filename_min <- paste(typemin,'_day_',slice,'_',Variant,'_',model,'_',as.character(timestart),'_',as.character(timeend),'.nc',sep = '')
          filename_max <- paste(typemax,'_day_',slice,'_',Variant,'_',model,'_',as.character(timestart),'_',as.character(timeend),'.nc',sep = '')
          myurl_min <- paste(ftp_min,timeyear,'.nc?','var=',type,'min','&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=',as.character(timestart),'T12%3A00%3A00Z&time_duration=P1D','&timeStride=1',sep = '')
          myurl_max <- paste(ftp_max,timeyear,'.nc?','var=',type,'max','&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=',as.character(timestart),'T12%3A00%3A00Z&time_duration=P1D','&timeStride=1',sep = '')
            # downloading file
            if(dir.exists('./temp/')==FALSE){dir.create('./temp/')}
            if(file.exists(paste('./temp/',filename_min,sep= ''))==FALSE|file.exists(paste('./temp/',filename_max,sep= ''))==FALSE){utils::download.file(quiet = T, method = 'curl', url = myurl_min, destfile = paste('./temp/',filename_min,sep= ''), mode = 'wb', extra = '-L');utils::download.file(quiet = T, method = 'curl', url = myurl_max, destfile = paste('./temp/',filename_max,sep= ''), mode = 'wb', extra = '-L')}
            # Reading the tmin ncdf file
            test4<-file.info(paste('./temp/',filename_min,sep= ''))$size
            stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test4 > 3.0e6)
            nc_min <- ncdf4::nc_open( paste('./temp/',filename_min,sep = '') )
            data_min <- ncdf4::ncvar_get(nc_min,typemin, start = c(1,1,1) , count = c(-1, -1 ,1))
            #transpose the data
            data_min <- raster::t(data_min)
            #reorder the rows
            data_min<-data_min[ nrow(data_min):1, ]
            ncdf4::nc_close(nc_min)
            # Reading the tmax ncdf file
            test5<-file.info(paste('./temp/',filename_max,sep= ''))$size
            stopifnot('The NEX GDDP server is temporarily unable to service your request due to maintenance downtime or capacity problems. Please try again later.' = test5 > 3.0e6)
            nc_max <- ncdf4::nc_open( paste('./temp/',filename_max,sep = '') )
            data_max <- ncdf4::ncvar_get(nc_max,typemax, start = c(1,1,1) , count = c(-1, -1 ,1))
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
    cat('Sorry!',paste(format(as.Date(start),'%b'),format(as.Date(start),'%Y'),sep=','),'is out of coverage for the NEX-GDDP-CMIP6 data products.','  \n')
    cat('Please pick start and end dates following notes described at the function notes to access the NEX-GDDP-CMIP6 data products.','  \n')
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
