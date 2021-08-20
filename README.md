
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NASAaccess

<!-- badges: start -->
<!-- badges: end -->

[*Ibrahim N.
Mohammed*](https://science.gsfc.nasa.gov/sed/bio/ibrahim.mohammed "Ibrahim N. Mohammed")

## **What is NASAaccess?**

*NASAaccess* is a software application in the form of a
[R](https://www.r-project.org/about.html) package and a web application.
*NASAaccess* software can generate gridded ascii tables of climate
[CMIP5](https://pcmdi.llnl.gov/mips/cmip5/ "Coupled Model Intercomparison Project Phase 5")
and weather data
([GPM](https://gpm.nasa.gov/data/directory "Global Precipitation Measurement"),
[TRMM](https://gpm.nasa.gov/missions/trmm "Tropical Rainfall Measuring Mission"),
[GLDAS](https://ldas.gsfc.nasa.gov/gldas "Global Land Data Assimilation System"))
needed to drive various hydrological models (e.g.,
[SWAT](https://swat.tamu.edu/ "Soil & Water Assessment Tool"),
[VIC](https://github.com/UW-Hydro/VIC "Variable Infiltration Capacity"),
[RHESSys](https://github.com/RHESSys/RHESSys "The Regional Hydro-Ecological Simulation System"),
…etc.).

## **Where to find the NASAaccess software?**

The *NASAaccess* R package is an open source software package under
[NASA Open Source Agreement
v1.3](https://opensource.org/licenses/NASA-1.3) and can be downloaded
from Github at <https://github.com/nasa/NASAaccess>.

*NASAaccess* is also available as a
[Tethys](https://www.tethysplatform.org/) web-based application that can
be installed on servers for multiple usage suited for agencies and
centers. Full details on installing the web-based version of
*NASAaccess* at local servers can be found at
[Here](https://github.com/BYU-Hydroinformatics/SWATOnline/blob/master/Documentation/nasaaccess%20-%20Installation%20Guide.pdf).

## **What is needed to install the NASAaccess software on my local machine?**

On a local machine the user should have installed the following programs
as well as setting up a user account. The list below gives a summary of
what is needed to be done prior to work with NASAaccess software on any
local machine:

-   Installing R software — <https://cloud.r-project.org/>

-   Installing Rstudio software — <https://rstudio.com/> (OPTIONAL)

-   *NASAaccess* R package needs a user registration access with
    [Earthdata](https://earthdata.nasa.gov/). Users should set up a
    registration account(s) with
    [Earthdata](https://earthdata.nasa.gov/) login as well as well as
    authorizing
    [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
    [GES DISC](https://disc.gsfc.nasa.gov/) data access. Please refer to
    <https://disc.gsfc.nasa.gov/data-access> for further details.

-   After registration with [Earthdata](https://earthdata.nasa.gov/)
    *NASAaccess* software package users should create a reference file
    (*netrc*) with [Earthdata](https://earthdata.nasa.gov/) credentials
    stored in it to streamline the retrieval access to
    [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
    servers.

    -   Creating the *.netrc* file at the user machine *Home* directory
        and storing the user
        [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
        [GES DISC](https://disc.gsfc.nasa.gov/) logging information in
        it is needed to execute the *NASAaccess* package commands.
        Accessing data at NASA servers is further explained at
        [Here](https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget).

    -   For Windows users the
        [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
        [GES DISC](https://disc.gsfc.nasa.gov/) logging information
        should be saved in a file **\_netrc** beside the **.netrc** file
        explained above.

-   Installing [*curl*](https://curl.haxx.se/) software . Since Mac
    users have [*curl*](https://curl.haxx.se/) as part of macOS build,
    Windows users should make sure that their local machines build have
    [*curl*](https://curl.haxx.se/) installed properly.

-   Checking if you can run [*curl*](https://curl.haxx.se/) from your
    command prompt. Type `curl --help` and you should see the help pages
    for the [*curl*](https://curl.haxx.se/) program once everything is
    defined correctly.

-   Within Rstudio or R terminal run the following commands to install
    *NASAaccess*:

    -   `library(devtools)`

    -   `install_github("nasa/NASAaccess")`

    -   `library(NASAaccess)`

Within the Rstudio help tab the user can verify that the package has
been installed and browse the help pages of the various functions of
*NASAaccess*.

## **How to cite the NASAaccess software?**

Mohammed, I. N., 2019, NASAaccess: Downloading and reformatting tool for
NASA earth observation data products \[software\]. National Aeronautics
and Space Administration, Goddard Space Flight Center, Greenbelt,
Maryland. <https://github.com/nasa/NASAaccess>