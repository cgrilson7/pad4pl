4PL Web App
================
Colin G. Wilson

Installing R
------------

First, download R from r-project.org:

-   To download for (Mac) OS X, go to [this page](http://mirrors.nics.utk.edu/cran/bin/macosx/) and click on the latest release .pkg link.

-   To download for Windows, go to [this page](http://mirrors.nics.utk.edu/cran/) and click the link at the top of the page ('Download R \[version\] for Windows').

Open the .pkg (Mac) or .exe (Windows) file and follow the installation wizard. You can install R wherever you'd like.

Starting R
----------

Once you've installed R, navigate to your Applications or Program Files and look for the program named simply 'R' or 'R Console'. The R logo looks like this:

<img src="rlogo.jpeg" width="50px" style="display: block; margin: auto;" />

When R starts up, you'll be greeted with a simple interface and some messages about the version of R you're using. Below those, you'll see a purple **&gt;** and a cursor awaiting your input. This is where you will type, or copy-and-paste the R code I've provided in this document.

Installing packages needed to run the app
-----------------------------------------

Before you can run the app, you need to install a few dependencies, which R calls 'packages'. You will only have to do this once. Simply cut and paste the following code into the R Console and hit 'Enter'.

``` r
packages_needed <- c("tidyverse","shiny","shinyFiles",
                     "shinycssloaders","DT","dr4pl","dplyr", "devtools")
if (length(setdiff(packages_needed, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_needed, rownames(installed.packages())))  
}
```

Running the app
---------------

Moving forward, this is the only line of code you need to run in order to start up the app:

``` r
shiny::runGitHub('pad4pl','cgrilson7')
```

It should open in whichever is your default browser. I find it runs best in Chrome, but you shouldn't have any issues as long as Javascript is enabled.

If you have any questions, or are having problems with the app, feel free to reach out to me at **<colingwilson7@gmail.com>**. Cheers!
