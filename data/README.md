This website is a summary of the ANPR traffic data recently made available by Bristol City Council
<br>
<br>
The data is available [on the Bristol Open Data Portal] (https://opendata.bristol.gov.uk/explore/dataset/fact-journey-hourly/information/?disjunctive.journeylinkdescription&sort=rollupdatetime)
<br>
<br>
A map is available [here](https://opendata.bristol.gov.uk/explore/dataset/fact-journey-hourly/map/?disjunctive.journeylinkdescription&sort=rollupdatetime&location=17,51.4529,-2.59279&basemap=jawg.streets).

## What does this website show
The raw data on the Open Data portal is very big (~3Gb) and the map does not show the distinction between routes particularly well, this website aims to allow more interactivity with this phenomonal dataset.
In addition a graph and summary tables are provided.

## What is the significance of the particular routes
This is not known.
<br>
<br>

This was built in [R](https://www.r-project.org) , an open source programming language using the [Shiny package](https://shiny.rstudio.com), a web application framework for R. The chart uses the [Openair package](https://davidcarslaw.github.io/openair/). Users will need to download [R](https://cran.uni-muenster.de/) and I suggest the use of [RStudio](https://www.rstudio.com). R is completely free to use.
