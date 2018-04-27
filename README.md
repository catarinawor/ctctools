# R package: ctctools 
A suite of R functions for analyses performed by the Chinook Technical Committee (CTC) of the Pacific Salmon Commission (PSC). Running the following two lines in R will install the `devtools` package from CRAN, then install `ctctools` from github. Not all functions have been fully tested. 

If you are using Ubuntu/Linux, for a successful install of devtools, you may need to run this line in BASH:

`sudo apt-get install libcurl4-openssl-dev libssl-dev`

Then in R:
    
```{r} 
install.packages("devtools") 
devtools::install_github("MichaelFolkes/ctctools") 
```

Once the package is installed, if using Rstudio, you will find some basic information in the user guides area of the package documentation. There is a file named `ctctools_introduction`.
Giving credit is nice. You can obtain citation details for this R package using the command: 
`citation('ctctools')`.
