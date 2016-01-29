## This is the address parser I need to place on GitHub and reference in my COPT code.

addressParser <- function(address) {
        
        #address <- "6711 Columbia Gateway Drive"
        
        #Output Fields
        street.number <- ""
        street.name <- ""
        street.type <- ""
        street.direction <- ""
        street.hemisphere <- ""
        street.parse.message <- ""
        
        #Constants and reference tables.
        quads <- c('NE', 'NW', 'SE', 'SW')
        hemis.input <- c('N', 'S', 'E', 'W', 'North', 'South', 'East', 'West')
        hemis.output <- c('North', 'South', 'East', 'West', 'North', 'South', 'East', 'West')
        highways <- c("Rt.", "Route")
        types.input <- c("Ave", "Ave.","Avenue","Boulevard", "Blvd.", "Blvd", "Circle", "Ct.", "Court", "Drive", "Dr", "Gateway", "Highway", "Lane", "Pkwy", "Parkway", "Rd.", "Rd", "Road", "St", "Street", "Street,", "Way")
        types.output <- c("Avenue","Avenue","Avenue","Boulevard", "Boulevard", "Boulevard", "Circle", "Court", "Court", "Drive", "Drive", "Gateway", "Highway", "Lane", "Parkway", "Parkway", "Road", "Road", "Road", "Street", "Street", "Street", "Way")
        types.df <- data.frame(types.input, types.output, stringsAsFactors = FALSE)
        
        #split the input string into groups/words.
        x <- unlist(strsplit(address, split = " "))
        #Stats about the split address.
        start <- 1
        end <- length(x)
        
        #street number should be the first field in a legitimate address.
        street.number <- x[1]
        
        #check for highway (vs. street) address as these follow a different pattern.
        if(length(which(highways %in% unlist(strsplit(address, " "))))>0) {
                street.name <- x[end]
                list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"="Route", "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
        } else {
                #Check the first field for all numerics
                if(length(grep("^[0-9]*$", street.number))==1) {
                        #good, the address starts with a street number.
                        #assume the address name starts at position 2, then test this assumption.
                        street.name.start <- 2
                        #Is there a hemisphere indicator where we expect street to start?
                        if(x[street.name.start] %in% hemis.input) {
                                #there is a city hemisphere designation in front of the street name.
                                #ex. 250 W Pratt St.
                                street.hemisphere <- hemis.output[which(hemis.input==x[street.name.start])]
                                street.name.start <- street.name.start + 1
                                #Need to find the end of the road name.  We assume it is one back from road type.
                                if(x[end] %in% types.input) {
                                        street.name.end <- end - 1
                                        street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                        street.type <- types.output[which(types.input==x[end])]
                                        list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                } else {
                                        #If the last word is not a recognized road type, hopefully it is a directional indicator.  Otherwise, we need to classify as not an address.
                                        if(x[end] %in% quads) {
                                                street.name.end <- end - 2
                                                street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                                street.type <- types.output[which(types.input==x[end-1])]
                                                street.direction <- x[end]
                                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                        } else {
                                                street.parse.message <- "Not an address"
                                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"="", "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                        }
                                }
                        } else {
                                #there is no city hemisphere designation before the street name.
                                #we then assume the street name starts right after street number.
                                #Need to find the end of the road name.  We assume it is one back from road type.
                                if(x[end] %in% types.input) {
                                        street.name.end <- end - 1
                                        street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                        street.type <- types.output[which(types.input==x[end])]
                                        list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                } else {
                                        #If the last word is not a recognized road type, hopefully it is a directional indicator.  Otherwise, we need to classify as not an address.
                                        if(x[end] %in% quads) {
                                                street.name.end <- end - 2
                                                street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                                street.type <- types.output[which(types.input==x[end-1])]
                                                street.direction <- x[end]
                                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                        } else {
                                                street.parse.message <- "Not an address"
                                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"="", "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                        }
                                }
                        }
                } else {
                        #Before we declare it not an address because the first string is not numeric...
                        #...we need to make sure it is not a numeric with some punctuation or string at the end.
                        
                        #The following further deconstructs the first string.
                        street.number <- unlist(strsplit(street.number, "-|[A-Z]$"))[1]
                        if(length(grep("^[0-9]*$", street.number))==1) {
                                #good, the address starts with a street number assuming we drop the add-on characters.
                                
                                #assume the address name starts at position 2, then test this assumption.
                                street.name.start <- 2
                                
                                #Is there a hemisphere indicator where we expect street to start?
                                if(x[street.name.start] %in% hemis.input) {
                                        #there is a city hemisphere designation in front of the street name.
                                        #ex. 250 W Pratt St.
                                        street.hemisphere <- hemis.output[which(hemis.input==x[street.name.start])]
                                        street.name.start <- street.name.start + 1
                                        #Need to find the end of the road name.  We assume it is one back from road type.
                                        if(x[end] %in% types.input) {
                                                street.name.end <- end - 1
                                                street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                                street.type <- types.output[which(types.input==x[end])]
                                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                        } else {
                                                #If the last word is not a recognized road type, hopefully it is a directional indicator.  Otherwise, we need to classify as not an address.
                                                if(x[end] %in% quads) {
                                                        street.name.end <- end - 2
                                                        street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                                        street.type <- types.output[which(types.input==x[end-1])]
                                                        street.direction <- x[end]
                                                        list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                                } else {
                                                        street.parse.message <- "Not an address"
                                                        list("address"=address, "street.parse.message"=street.parse.message, "street.number"="", "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                                }
                                        }
                                } else {
                                        #there is no city hemisphere designation before the street name.
                                        #we then assume the street name starts right after street number.
                                        #Need to find the end of the road name.  We assume it is one back from road type.
                                        if(x[end] %in% types.input) {
                                                street.name.end <- end - 1
                                                street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                                street.type <- types.output[which(types.input==x[end])]
                                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                        } else {
                                                #If the last word is not a recognized road type, hopefully it is a directional indicator.  Otherwise, we need to classify as not an address.
                                                if(x[end] %in% quads) {
                                                        street.name.end <- end - 2
                                                        street.name <- paste(x[street.name.start:street.name.end], collapse = " ")
                                                        street.type <- types.output[which(types.input==x[end-1])]
                                                        street.direction <- x[end]
                                                        list("address"=address, "street.parse.message"=street.parse.message, "street.number"=street.number, "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                                } else {
                                                        street.parse.message <- "Not an address"
                                                        list("address"=address, "street.parse.message"=street.parse.message, "street.number"="", "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                                                }
                                        }
                                }
                        } else {
                                street.parse.message <- "Not an address"
                                list("address"=address, "street.parse.message"=street.parse.message, "street.number"="", "street.name"=street.name, "street.type"=street.type, "street.hemisphere"=street.hemisphere, "street.direction"=street.direction)
                        }
                }
        }
}