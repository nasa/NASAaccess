###code to save user credentials into netrc file
library(getPass)


repeat {

  login<-getPass::getPass(msg="What is your Earthdata GES DISC Login: ", noblank = TRUE)
  if(is.null(login)==TRUE)
    {
      rm(login)
      stop(
        c(
        cat('**You need to provide your Earthdata GES DISC login to proceed**',' \n'),
        cat('NASAaccess package needs a user registration access with Earthdata. Users should set up a registration account(s) with Earthdata login as well as well as authorizing NASA GES DISC data access.',' \n'),
        cat('Please refer to https://disc.gsfc.nasa.gov/data-access for further details.',' \n'),
        cat('**Please retry after obtaining Earthdata GES DISC login and password information as well as authorizing NASA GES DISC data access**',' \n')),call. = T)
    }

  login2<- getPass::getPass(msg="To confirm your Login information, please reenter your Earthdata GES DISC Login: ")


  if(identical(login,login2) == FALSE)
  {
    print('Your Earthdata GES DISC login information entries are not identical!
            Make sure to enter your Earthdata GES DISC login information correctly.')
  }
  if(identical(login,login2) == TRUE)
  {
    break
  }
}


repeat {

  password<-getPass::getPass(msg= "Enter your Earthdata GES DISC Login password: ", noblank = TRUE)
  if(is.null(password)==TRUE)
  {
    rm(login,login2,password)
    stop(
      c(
        cat('**You need to provide your Earthdata GES DISC password to proceed**',' \n'),
        cat('NASAaccess package needs a user registration access with Earthdata. Users should set up a registration account(s) with Earthdata login as well as well as authorizing NASA GES DISC data access.',' \n'),
        cat('Please refer to https://disc.gsfc.nasa.gov/data-access for further details.',' \n'),
        cat('**Please retry after obtaining Earthdata GES DISC login and password information as well as authorizing NASA GES DISC data access**',' \n')),call. = T)

  }
  password2<- getPass::getPass("To confirm your Login password information, please reenter your Earthdata GES DISC Login password: ")

  if(identical(password,password2) == FALSE)
  {
    print('Your Earthdata GES DISC login password entries are not identical!
            Make sure to enter your Earthdata GES DISC login password correctly.')
  }
  if(identical(password,password2) == TRUE)
  {
    break
  }
}

rm(login2,password2)


writeLines(paste('machine', 'urs.earthdata.nasa.gov', 'login', login, 'password', password, sep = " "),'~/.netrc')
cat("", file = '~/.urs_cookies')
##check the machine type to decide on _netrc file creation
if(.Platform$OS.type != "unix")
{
  writeLines(paste('machine', 'urs.earthdata.nasa.gov', 'login', login, 'password', password, sep = " "),'~/_netrc')
}

rm(login,password)


