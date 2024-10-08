% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HSIeqtn.R
\name{HSIeqtn}
\alias{HSIeqtn}
\title{Computes Habitat Suitability Index based on Model-Specified Equation}
\usage{
HSIeqtn(HSImodelname, SIV, HSImetadata, exclude = NULL)
}
\arguments{
\item{HSImodelname}{a character string in quotations that must match an existing model
name in HSImetadata.}

\item{SIV}{a vector of suitability index values used in the model specified in HSImodelname.}

\item{HSImetadata}{a data frame of HSI model metadata within the ecorest package.}

\item{exclude}{a list of character strings specifying components to be excluded from calculations.}
}
\value{
A numeric of the habitat suitability index ranging from 0 to 1.
}
\description{
\code{HSIeqtn} computes a habitat suitability index based on equations specified
 in U.S. Fish and Wildlife Service habitat suitability models contained within ecorest
 via HSImodels and HSImetadata. Habitat suitability indices represent an overall assessment
 of habitat quality from combining individual suitability indices for multiple independent
 variables. The function computes an overall habitat suitability index.
}
\examples{

#Compute patch quality for the Barred Owl model (no components)
#Allen A.W. 1982. Habitat Suitability Index Models: Barred owl. FWS/OBS 82/10.143.
#U.S. Fish and Wildlife Service. https://pubs.er.usgs.gov/publication/fwsobs82_10_143.
#Suitability indices relate to density of large trees, mean diameter of overstory trees,
#and percent canopy cover of overstory.
#Example suitability vectors
HSIeqtn("barredowl", c(1,1,1), HSImetadata) #c(1,1,1) should result in 1.00
HSIeqtn("barredowl", c(0.5,1,1), HSImetadata) #c(0.5,1,1) should result in 0.707
HSIeqtn("barredowl", c(0,1,1), HSImetadata) #c(0,1,1) should result in 0.00
HSIeqtn("barredowl", c(0,NA,1), HSImetadata) #c(0,NA,1) should return error message
HSIeqtn("barredowl", c(NA,1,1,1), HSImetadata) #c(NA,1,1,1) should return error message

#Compute patch quality for the Juvenile Alewife model (two components)
#Pardue, G.B. 1983. Habitat Suitability index models: alewife and blueback herring.
#U.S. Dept. Int. Fish Wildl. Serv. FWS/OBS-82/10.58. 22pp.
#Suitability indices relate to zooplankton density, salinity, and water temperature
#Example suitability vectors are c(1,1,1), c(0.5,1,1), and c(0,1,1)
HSIeqtn("alewifeJuv", c(1,1,1), HSImetadata) #c(1,1,1) should result in 1.00
HSIeqtn("alewifeJuv", c(0.5,1,1), HSImetadata) #c(0.5,1,1) should result in 0.50
HSIeqtn("alewifeJuv", c(0,1,1), HSImetadata) #c(0,1,1) should result in 0.00
HSIeqtn("alewifeJuv", c(1,NA,1), HSImetadata) #c(1,NA,1) returns error message
HSIeqtn("alewifeJuv", c(1,1,1,NA), HSImetadata) #c(1,1,1,NA) returns error message

#Compute patch quality for Cutthroat trout model for lacustrine habitats (7 components)
#with spawning and lacustrine habitat and with only lacustrine habitat (i.e., 
#embryo component is excluded).
#Hickman, T., and R.F. Raleigh. 1982. Habitat suitability index models: 
#Cutthroat trout. U.S.D.I. Fish and Wildlife Service. FWS/OBS-82/10.5. 38 pp.
#Suitability indices relate to temperature during the warmest period of the year,
#maximum temperature during embryo development, minimum dissolved oxygen during
#the late growing season, average velocity over spawning areas, average size 
#of substrate in spawning areas, annual maximal or minimal pH, and percent fines
#in the spawning area.
#Example suitability vectors are c(1,1,1,1,1,1,1), c(0.5,1,0.5,0,1,1,1) and c(1,NA,0.5,NA,NA,0.5,NA)
#c(1,1,1,1,1,1,1) should result in 1
HSIeqtn("cutthroatLacGenLtoe15C", c(1,1,1,1,1,1,1), HSImetadata) 
#c(0.5,1,0.5,0,1,1,1) should result in 0
HSIeqtn("cutthroatLacGenLtoe15C", c(0.5,1,0.5,0,1,1,1), HSImetadata) 
#c(1,NA,0.5,NA,NA,0.5,NA) should result in 0.63
HSIeqtn("cutthroatLacGenLtoe15C", c(1,NA,0.5,NA,NA,0.5,NA), HSImetadata, exclude=c("CE")) 

}
\references{
US Fish and Wildlife Service. (1980). Habitat as a basis for environmental assessment.
Ecological Services Manual, 101.

US Fish and Wildlife Service. (1980). Habitat Evaluation Procedures (HEP).
Ecological Services Manual, 102.

US Fish and Wildlife Service. (1981). Standards for the Development of
Habitat Suitability Index Models. Ecological Services Manual, 103.
}
