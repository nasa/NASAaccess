
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NASAaccess

<!-- badges: start -->

[![Anaconda-Server
Badge](https://anaconda.org/conda-forge/r-nasaaccess/badges/version.svg)](https://anaconda.org/conda-forge/r-nasaaccess)
[![Anaconda-Server
Badge](https://anaconda.org/conda-forge/r-nasaaccess/badges/downloads.svg)](https://anaconda.org/conda-forge/r-nasaaccess)
[![Anaconda-Server
Badge](https://anaconda.org/conda-forge/r-nasaaccess/badges/platforms.svg)](https://anaconda.org/conda-forge/r-nasaaccess)
[![](https://img.shields.io/readthedocs/nasaaccess?style=social)](https://nasaaccess.readthedocs.io/en/latest/index.html)
[![](https://img.shields.io/github/stars/nasa/nasaaccess?style=social)](https://github.com/nasa/NASAaccess)

<!-- ![lifecycle](https://img.shields.io/badge/lifecycle-stable-sucess.svg?style=plastic&logo=appveyor)
<!-- ![GitHub Downloads](https://img.shields.io/github/downloads/nasa/NASAaccess/total?style=plastic)

<!-- badges: end -->

[*Ibrahim N.
Mohammed*](https://science.gsfc.nasa.gov/sed/bio/ibrahim.mohammed "Ibrahim N. Mohammed")

## **What is NASAaccess?**

*NASAaccess* is a software application in the form of a
[R](https://www.r-project.org/about.html) package, a
[conda](https://docs.conda.io/en/latest/) package, and a
[Tethys](https://www.tethysplatform.org/) web application. *NASAaccess*
software can generate gridded ascii tables of climate
[CMIP5](https://pcmdi.llnl.gov/mips/cmip5/ "Coupled Model Intercomparison Project Phase 5"),
[CMIP6](https://pcmdi.llnl.gov/CMIP6/ "Coupled Model Intercomparison Project Phase 6"),
and earth observation remote sensing data
([GPM](https://gpm.nasa.gov/data/directory "Global Precipitation Measurement"),
[TRMM](https://gpm.nasa.gov/missions/trmm "Tropical Rainfall Measuring Mission"),
[GLDAS](https://ldas.gsfc.nasa.gov/gldas "Global Land Data Assimilation System"))
needed to drive various hydrological models (e.g.,
[SWAT](https://swat.tamu.edu/ "Soil & Water Assessment Tool"),
[VIC](https://github.com/UW-Hydro/VIC "Variable Infiltration Capacity"),
[RHESSys](https://github.com/RHESSys/RHESSys "The Regional Hydro-Ecological Simulation System"),
…etc.). The *NASAaccess* Tethys web-based application can be used for
accessing, reformatting, and visualizing climate and earth observation
remote sensing gridded time series data as well.

## **Where to find the NASAaccess software?**

- R package can be downloaded from GitHub at
  <https://github.com/nasa/NASAaccess>.

- Conda package can be installed directly from *Anaconda* by searching
  for `r-nasaaccess`.

- Tethys web-based application can directly installed from GitHub at
  <https://github.com/imohamme/tethys_nasaaccess>.

## **How NASAaccess software is distributed?**

*NASAaccess* is an open-source software package under [NASA Open Source
Agreement v1.3](https://opensource.org/license/nasa1-3-php/).

## **What is needed to install the NASAaccess software on my local machine?**

### **R Library**

On a local machine the user should have installed the following programs
as well as setting up a user account. The list below gives a summary of
what is needed to be done prior to work with NASAaccess software on any
local machine:

- [Installing R software](https://www.r-project.org/)

- [Installing Rstudio software](https://posit.co/) (OPTIONAL)

- *NASAaccess* R package needs a user registration access with
  [Earthdata](https://www.earthdata.nasa.gov/). Users should set up a
  registration account(s) with
  [Earthdata](https://www.earthdata.nasa.gov/) login as well as well as
  authorizing
  [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
  [GES DISC](https://disc.gsfc.nasa.gov/) data access. Please refer to
  <https://disc.gsfc.nasa.gov/data-access> for further details.

- Installing [*curl*](https://curl.se/) software . Since Mac users have
  [*curl*](https://curl.se/) as part of macOS build, Windows users
  should make sure that their local machines build have
  [*curl*](https://curl.se/) installed properly.

- Checking if you can run [*curl*](https://curl.se/) from your command
  prompt. Type `curl --help` and you should see the help pages for the
  [*curl*](https://curl.se/) program once everything is defined
  correctly.

- Within Rstudio or R terminal run the following commands to install
  *NASAaccess*:

  - `library(devtools)`

  - `install_github("nasa/NASAaccess", build_vignettes = TRUE)`

  - `library(NASAaccess)`

Within the Rstudio help tab the user can verify that the package has
been installed and browse the help pages of the various functions of
*NASAaccess*.

### **Conda Package**

- After registration with [Earthdata](https://www.earthdata.nasa.gov/)
  *NASAaccess* software package users should create a reference file
  (*.netrc*) with [Earthdata](https://www.earthdata.nasa.gov/)
  credentials stored in it to streamline the retrieval access from
  [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
  servers.

  - Creating the *.netrc* file at the user machine *Home* directory and
    storing the user
    [NASA](https://www.nasa.gov/ "The National Aeronautics and Space Administration")
    [GES DISC](https://disc.gsfc.nasa.gov/) logging information in it is
    needed to execute the *NASAaccess* package commands. Accessing data
    from NASA servers is further explained at
    [Here](https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget).

- To install *NASAaccess* package in a conda environment run the
  following:

``` python
conda install -c conda-forge r-nasaaccess
```

### **Tethys web-based Application**

Full details on installing the web-based application of *NASAaccess* on
single machines and local servers can be found at
[readthedocs](https://nasaaccess.readthedocs.io/en/latest/nasaaccess_tethys.html).

## **Is there a walk through examples for NASAaccess software?**

Software users are encouraged to visit
(<https://imohamme.github.io/NASAaccess/>) to learn more on *NASAaccess*
functionality and capabilities.

## **How to cite R NASAaccess package?**

``` r
citation(package = 'NASAaccess')
#> 
#> To cite package 'NASAaccess' in publications use:
#> 
#>   Mohammed I (2023). _NASAaccess: Downloading and Reformatting Tool for
#>   NASA Earth Observation Data Products_. R package version 3.3.1,
#>   <https://github.com/nasa/NASAaccess>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {{NASAaccess}: Downloading and Reformatting Tool for NASA Earth Observation Data Products},
#>     author = {Ibrahim Mohammed},
#>     year = {2023},
#>     institution = {National Aeronautics and Space Administration, Goddard Space Flight Center},
#>     address = {Greenbelt, Maryland},
#>     note = {R package version 3.3.1},
#>     url = {https://github.com/nasa/NASAaccess},
#>   }
```
