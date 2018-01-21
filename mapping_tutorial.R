
setwd("/home/you/mapping_tutorial")
#install neccesary packages 
install.packages(c("cartography", "rgdal", "sp"))

library(cartography) #read in map-making package
library(rgdal) #read in general GIS package 
library(stringr) #read in string manipulation package 


#ownership registrations 
owners <- read.csv("data/owners.csv")
#building data 
bldgs <- read.csv("data/bldg.csv")

head(owners)


owner_count <- aggregate(RegistrationID ~ RegistrationContactID, data=owners, FUN=length)
names(owner_count) <- c("RegistrationContactID", "Building_Count")
head(owner_count)
nrow(owner_count[owner_count$Building_Count > 2 ,])


org_agg <- aggregate(RegistrationID ~ CorporationName, data=owners, FUN=length)
nrow(org_agg[org_agg$RegistrationID > 2 ,])
org_agg <- aggregate(RegistrationID ~ CorporationName, data=owners, FUN=length)
names(org_agg) <- c("CorporationName", "Building_Count")
nrow(org_agg[org_agg$Building_Count > 2 ,])


org_agg$CorporationName<- gsub("[^[:alnum:][:blank:]]", "", org_agg$CorporationName)
org_agg <- org_agg[org_agg$CorporationName !="" ,]
head(org_agg)
nrow(org_agg[org_agg$Building_Count > 2 ,])



owners$RealID <- paste(owners$BusinessHouseNumber, owners$BusinessStreetName, sep=" ")
real_agg <- aggregate(RegistrationID ~ RealID, data=owners, FUN=length)
names(real_agg) <- c("RealID", "Building_Count")
nrow(real_agg[real_agg$Building_Count > 2 ,])


summary(owners$Type)

owners <- owners[! duplicated(owners$RegistrationID) ,]
real_agg <- aggregate(RegistrationID ~ RealID, data=owners, FUN=length)
names(real_agg) <- c("RealID", "Building_Count")
nrow(real_agg[real_agg$Building_Count > 2 ,])


#merge building count with owner DF
owners <- merge(x=owners, y=real_agg, by="RealID")

head(bldgs)

bldg_counts <- merge(x=bldgs, y=owners, by="RegistrationID")

bldg_counts <- bldg_counts[! is.na(bldg_counts$Building_Count) ,]

head(bldg_counts)


pluto_bk <- read.csv("data/pluto.csv")
names(pluto_bk)

pluto_bk <- pluto_bk[, c(4, 5, 6, 58,71, 73, 74, 75)]
names(pluto_bk)

#subset dataframe for just Brooklyn 
bldg_counts  <- bldg_counts [bldg_counts $BoroID == 3 ,]

#create new columns with padded values 
bldg_counts ["New_Block"] <- lapply(bldg_counts ["Block"], function(x) sprintf("%05d", x))
bldg_counts ["New_Lot"] <- lapply(bldg_counts ["Lot"], function(x) sprintf("%04d", x))

#use paste function to combine everything into one variable 
bldg_counts ["BBL"] <- as.numeric(paste(bldg_counts $BoroID, bldg_counts $New_Block, bldg_counts $New_Lot, sep=""))

head(bldg_counts$BBL, n=15)

names(pluto_bk)
pluto_bk <- pluto_bk[, c(5, 2)]
nrow(bldg_counts)

bldg_counts <- merge(x=bldg_counts, y=pluto_bk, by="BBL")
nrow(bldg_counts)


multiple <- bldg_counts[bldg_counts$Building_Count > 2 ,]
tract_counts <- aggregate(Building_Count ~ CT2010, data=multiple, FUN=length )
names(tract_counts)[1] <- "CTLabel"
nrow(tract_counts)
length(unique(pluto_bk$CT2010))

bk_shape <- readOGR(dsn = "shapefiles", layer = "nyct2010")
bk_shape <- bk_shape[bk_shape@data$BoroCode == "3" ,]
head(tract_counts)

require(sp)
bk_shape <- merge(bk_shape, tract_counts, by="CTLabel")

head(bk_shape@data)


plot(bk_shape, border = NA, col = NA, bg = "#A6CAE0")
plot(bk_shape, col  = "#E3DEBF", border=NA, add=TRUE)

cols <- carto.pal(pal1 = "green.pal" ,n1 = 8)
choroLayer(spdf = bk_shape, 
           df = bk_shape@data, 
           var = "Building_Count", 
           breaks = c(0, 10, 30, 50, 70, 90, 120, 150), 
           col = cols, 
           border = "grey40", 
           lwd = 0.5, 
           legend.pos = "left",
           legend.title.txt = "Number of Buildings", 
           legend.values.rnd = 10,
           add = TRUE) 

layoutLayer(title = "Census Tracts by Building Ownership Concentration", 
            author = "YourName",  
            sources = "Source: NYC OpenData", 
            scale = NULL, 
            col = NA, 
            coltitle = "black", 
            frame = FALSE,  
            bg = NA)

