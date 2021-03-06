#' Computes Habitat Suitability Index based on Model-Specified Equation
#'
#' \code{HSIeqtn} computes a habitat suitability index based on equations specified
#'  in U.S. Fish and Wildlife Service habitat suitability models contained within ecorest
#'  via HSImodels and HSImetadata. Habitat suitability indices represent an overall assessment
#'  of habitat quality from combining individual suitability indices for multiple independent
#'  variables. The function computes an overall habitat suitability index.
#'
#' @param HSImodelname a character string in quotations that must match an existing model
#'  name in HSImetadata.
#' @param SIV a vector of suitability index values used in the model specified in HSImodelname.
#' @param HSImetadata a data frame of HSI model metadata within the ecorest package.
#'
#' @return A numeric of the habitat suitability index ranging from 0 to 1.
#'
#' @references
#' US Fish and Wildlife Service. (1980). Habitat as a basis for environmental assessment.
#' Ecological Services Manual, 101.
#'
#' US Fish and Wildlife Service. (1980). Habitat Evaluation Procedures (HEP).
#' Ecological Services Manual, 102.
#'
#' US Fish and Wildlife Service. (1981). Standards for the Development of
#' Habitat Suitability Index Models. Ecological Services Manual, 103.
#'
#' @examples
#'
#' #Compute patch quality for the Barred Owl model (no components)
#' #Allen A.W. 1982. Habitat Suitability Index Models: Barred owl. FWS/OBS 82/10.143.
#' #U.S. Fish and Wildlife Service. https://pubs.er.usgs.gov/publication/fwsobs82_10_143.
#' #Suitability indices relate to density of large trees, mean diameter of overstory trees,
#' #and percent canopy cover of overstory.
#' #Example suitability vectors
#' HSIeqtn("barredowl", c(1,1,1), HSImetadata) #c(1,1,1) should result in 1.00
#' HSIeqtn("barredowl", c(0.5,1,1), HSImetadata) #c(0.5,1,1) should result in 0.707
#' HSIeqtn("barredowl", c(0,1,1), HSImetadata) #c(0,1,1) should result in 0.00
#' HSIeqtn("barredowl", c(0,NA,1), HSImetadata) #c(0,NA,1) should return error message
#' HSIeqtn("barredowl", c(NA,1,1,1), HSImetadata) #c(NA,1,1,1) should return error message
#'
#' #Compute patch quality for the Juvenile Alewide mode (two components)
#' #Pardue, G.B. 1983. Habitat Suitability index models: alewife and blueback herring.
#' #U.S. Dept. Int. Fish Wildl. Serv. FWS/OBS-82/10.58. 22pp.
#' #Suitability indices relate to zooplankton density, salinity, and water temperature
#' #Example suitability vectors are c(1,1,1), c(0.5,1,1), and c(0,1,1)
#' HSIeqtn("alewifeJuv", c(1,1,1), HSImetadata) #c(1,1,1) should result in 1.00
#' HSIeqtn("alewifeJuv", c(0.5,1,1), HSImetadata) #c(0.5,1,1) should result in 0.50
#' HSIeqtn("alewifeJuv", c(0,1,1), HSImetadata) #c(0,1,1) should result in 0.00
#' HSIeqtn("alewifeJuv", c(1,NA,1), HSImetadata) #c(1,NA,1) returns error message
#' HSIeqtn("alewifeJuv", c(1,1,1,NA), HSImetadata) #c(1,1,1,NA) returns error message
#'
#' @export
HSIeqtn <- function(HSImodelname, SIV, HSImetadata){
  # Find the location of the model in HSImetadata
    model.loc <- which(HSImetadata$model == HSImodelname)

  # Isolate input variables and assign generic naming
    SIV.model <- which(!is.na(HSImetadata[model.loc,9:30]))
    SIV.name.gen <- paste("SIV",SIV.model, sep="")

  # Set names for outputs
    var.name <- c(SIV.name.gen, "CF", "CC","CCF","CWF","CW",
                "CCB","CCN","CWQ","CR","CCR","CD","COT","CL","CEL","CE",
                "CJ","CFr","CS","CA","CI","CNI","CWFC","CT","CJA","Eqtn")

  # Create an empty list with entries named from var.name
    HSI <- vector("list", length = length(var.name))
    names(HSI) <- var.name

  # Insert equation text into each list location
    HSI$CF <- parse(text=paste(HSImetadata$CF[model.loc]))
    HSI$CC <- parse(text=paste(HSImetadata$CC[model.loc]))
    HSI$CCF <- parse(text=paste(HSImetadata$CCF[model.loc]))
    HSI$CWF <- parse(text=paste(HSImetadata$CWF[model.loc]))
    HSI$CW <- parse(text=paste(HSImetadata$CW[model.loc]))
    HSI$CCB <- parse(text=paste(HSImetadata$CCB[model.loc]))
    HSI$CCN <- parse(text=paste(HSImetadata$CCN[model.loc]))
    HSI$CWQ <- parse(text=paste(HSImetadata$CWQ[model.loc]))
    HSI$CR <- parse(text=paste(HSImetadata$CR[model.loc]))
    HSI$CCR <- parse(text=paste(HSImetadata$CCR[model.loc]))
    HSI$CD <- parse(text=paste(HSImetadata$CD[model.loc]))
    HSI$COT <- parse(text=paste(HSImetadata$COT[model.loc]))
    HSI$CL <- parse(text=paste(HSImetadata$CL[model.loc]))
    HSI$CEL <- parse(text=paste(HSImetadata$CEL[model.loc]))
    HSI$CE <- parse(text=paste(HSImetadata$CE[model.loc]))
    HSI$CJ <- parse(text=paste(HSImetadata$CJ[model.loc]))
    HSI$CFr <- parse(text=paste(HSImetadata$CFr[model.loc]))
    HSI$CS <- parse(text=paste(HSImetadata$CS[model.loc]))
    HSI$CA <- parse(text=paste(HSImetadata$CA[model.loc]))
    HSI$CI <- parse(text=paste(HSImetadata$CI[model.loc]))
    HSI$CNI <- parse(text=paste(HSImetadata$CNI[model.loc]))
    HSI$CWFC <- parse(text=paste(HSImetadata$CWFC[model.loc]))
    HSI$CT <- parse(text=paste(HSImetadata$CT[model.loc]))
    HSI$CJA <- parse(text=paste(HSImetadata$CJA[model.loc]))
    HSI$Eqtn <- parse(text=paste(HSImetadata$Eqtn[model.loc]))

  # Populate the list with input suitability index values
    for(i in 1:length(SIV)){HSI[[i]] <- SIV[i]}

  # Create a list for outputs with the same structure and headings
    HSI.out <- HSI

  # Evaluate the HSI components and store in the new list
    j <- length(SIV.model)
    HSI.out[[j+1]] <- with(HSI,eval(HSI$CF))
    HSI.out[[j+2]] <- with(HSI,eval(HSI$CC))
    HSI.out[[j+3]] <- with(HSI,eval(HSI$CCF))
    HSI.out[[j+4]] <- with(HSI,eval(HSI$CWF))
    HSI.out[[j+5]] <- with(HSI,eval(HSI$CW))
    HSI.out[[j+6]] <- with(HSI,eval(HSI$CCB))
    HSI.out[[j+7]] <- with(HSI,eval(HSI$CCN))
    HSI.out[[j+8]] <- with(HSI,eval(HSI$CWQ))
    HSI.out[[j+9]] <- with(HSI,eval(HSI$CR))
    HSI.out[[j+10]] <- with(HSI,eval(HSI$CCR))
    HSI.out[[j+11]] <- with(HSI,eval(HSI$CD))
    HSI.out[[j+12]] <- with(HSI,eval(HSI$COT))
    HSI.out[[j+13]] <- with(HSI,eval(HSI$CL))
    HSI.out[[j+14]] <- with(HSI,eval(HSI$CEL))
    HSI.out[[j+15]] <- with(HSI,eval(HSI$CE))
    HSI.out[[j+16]] <- with(HSI,eval(HSI$CJ))
    HSI.out[[j+17]] <- with(HSI,eval(HSI$CFr))
    HSI.out[[j+18]] <- with(HSI,eval(HSI$CS))
    HSI.out[[j+19]] <- with(HSI,eval(HSI$CA))
    HSI.out[[j+20]] <- with(HSI,eval(HSI$CI))
    HSI.out[[j+21]] <- with(HSI,eval(HSI$CNI))
    HSI.out[[j+22]] <- with(HSI,eval(HSI$CWFC))
    HSI.out[[j+23]] <- with(HSI,eval(HSI$CT))
    HSI.out[[j+24]] <- with(HSI,eval(HSI$CJA))

  # Evaluate the overall HSI equation
    HSI.out[[j+25]] <- with(HSI.out,eval(HSI.out$Eqtn))

  # Remove unused components and reclassify as a data frame
    HSI.out2 <- HSI.out[which(!is.na(HSI.out))]
    HSI.out3 <- data.frame(HSI.out2)

  # Set error message for incorrect SIV length or NA input
    if(length(SIV.model) != length(SIV)){
      HSI.out4 <- "SIV vector length does not match equation."
    } else {
      HSI.out4 <- ifelse(is.numeric(HSI.out3$Eqtn),HSI.out3$Eqtn,"NA with possible SIV input error.")
    }

  # Return HSI outcome
    return(HSI.out4)
}
