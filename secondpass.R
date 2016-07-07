################################################################################
##  See which packages are installed. Then, list which packagess are needed.
##  Install those that are not there already, and then load them.

get.installed.package.list <- function() {
    package_frame <- installed.packages()
    package_list <- package_frame[, 1]
    return(unique(package_list))
}
old.package.list <- get.installed.package.list()
new.package.list <- c( "dplyr", "ggplot2" , "readr" , "stringr" )
for (i in new.package.list) {
    if (i %in% old.package.list == FALSE) {
        install.packages(i , dependencies = TRUE)
    }   
    require(i , character.only = TRUE)
}
rm( i )

################################################################################
##  See if we've already downloaded the data. If not, do so. Read in using
##  the 'readr' package by Hadley Wickham, so "read_csv()" instead of the 
##  usual "read.csv()."

if(file.exists("StormData.csv.bz2")==FALSE) {
    download.file(
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" ,
        destfile = "StormData.csv.bz2"
    )
}
stormdata <- read_csv( "stormdata.csv.bz2" )

################################################################################
##  Trim the dataset down to the few relevant columns for the 
##  purposes of this analysis. We could delete or overwrite the larger set, but 
##  it isn't so large that this is necessary with typical system resources.

thindata <- select(stormdata , EVTYPE , FATALITIES:PROPDMGEXP )

################################################################################
##  "PROPDMGEXP" represents the magitude of property damage (PROPDMG), a
##  multiplier. Create a new variable "MAGNITUDE" to properly quantify this. 
##  Then we'll create another variable "ADJUSTED" to capture a scaled value for 
##  property damage that is plottable.

thindata <- mutate( thindata , "MAGNITUDE" = str_to_lower(PROPDMGEXP) )
oldmag <- unique( thindata$MAGNITUDE )
newmag <- c( 1e3 , 1e6 , 1 , 1e9, 1 , 1 , 1e5 , 1e6 , 1 , 1e4 , 1e2 , 1e3 , 1e2,
             1e7 , 1 , 1e1 , 1e8 )
for( i in 1:17 ){
    thindata <- mutate( thindata , MAGNITUDE = replace( MAGNITUDE , MAGNITUDE ==
                                                        oldmag[i] , newmag[i]))
}
thindata <- mutate( thindata , MAGNITUDE = replace ( MAGNITUDE , 
                                                     is.na(MAGNITUDE) , 1 ))
thindata$MAGNITUDE <- as.numeric(thindata$MAGNITUDE)
thindata <- mutate( thindata , ADJUSTED = PROPDMG * MAGNITUDE)

################################################################################
##  "permitted" -- "The only events permitted in Storm Data are listed in Table
##  1 of Section 2.1.1" of  NATIONAL WEATHER SERVICE INSTRUCTION 10-16. There
##  are 48 of them.
permitted <- c( "Astronomical Low Tide" , "Avalanche" , "Blizzard" , 
                "Coastal Flood" , "Cold/Wind Chill" , "Debris Flow" , 
                "Dense Fog" , "Dense Smoke" , "Drought" , "Dust Devil" , 
                "Dust Storm" , "Excessive Heat" , "Extreme Cold/Wind Chill" , 
                "Flash Flood" , "Flood" , "Frost/Freeze" , "Funnel Cloud" , 
                "Freezing Fog" , "Hail" , "Heat" , "Heavy Rain" , "Heavy Snow" ,
                "High Surf" , "High Wind" , "Hurricane (Typhoon)" , "Ice Storm", 
                "Lake-Effect Snow" , "Lakeshore Flood" , "Lightning" , 
                "Marine Hail" , "Marine High Wind" , "Marine Strong Wind" , 
                "Marine Thunderstorm Wind" , "Rip Current" , "Seiche" , "Sleet"
                , "Storm Surge/Tide" , "Strong Wind" , "Thunderstorm Wind" , 
                "Tornado" , "Tropical Depression" , "Tropical Storm" , "Tsunami"
                , "Volcanic Ash" , "Waterspout" , "Wildfire" , "Winter Storm" , 
                "Winter Weather" )
permitted <- str_to_lower( permitted )

################################################################################
##  There are many typos and mis-labeled events in the dataset. We must massage
##  them into the permitted 48 as appropriate, and drop the ones that do not
##  belong.
thindata$EVTYPE <- str_to_lower(thindata$EVTYPE)
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("aval", (sort(unique(thindata$EVTYPE))))] , "avalanche"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("bliz", (sort(unique(thindata$EVTYPE))))] , "blizzard"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*coastal*flo*"),(sort(unique(thindata$EVTYPE))))] , "coastal flood"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*ex*cold*"),(sort(unique(thindata$EVTYPE))))] , "extreme cold/wind chill"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*ex*wind*"),(sort(unique(thindata$EVTYPE))))] , "extreme cold/wind chill"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% c("severe cold","record cold") , "extreme cold/wind chill"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% c("cold","cold wave","cold weather","cold wind chill temperatures","cold temperatures","cold temperature","cold/winds") , "extreme cold/wind chill"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("drought",(sort(unique(thindata$EVTYPE))))] , "drought"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("dust dev",(sort(unique(thindata$EVTYPE))))] , "dust devil"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*dust*storm*"),(sort(unique(thindata$EVTYPE))))] , "dust storm"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*ex*heat*"),(sort(unique(thindata$EVTYPE))))] , "excessive heat"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("flash", (sort(unique(thindata$EVTYPE))))] , "flash flood"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*lake*flood*"),(sort(unique(thindata$EVTYPE))))] , "lakeshore flood"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*lake*snow*"),(sort(unique(thindata$EVTYPE))))] , "lakeshore flood"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("lightn",(sort(unique(thindata$EVTYPE))))] , "lightning"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("marine tstm wind",(sort(unique(thindata$EVTYPE))))] , "marine thunderstorm wind"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("rip",(sort(unique(thindata$EVTYPE))))] , "rip current"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("sleet",(sort(unique(thindata$EVTYPE))))] , "sleet"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("surge",(sort(unique(thindata$EVTYPE))))] , "storm surge/tide"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*strong*wind*"),(sort(unique(thindata$EVTYPE))))] , "strong wind"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*th*wind*"),(sort(unique(thindata$EVTYPE))))] , "thunderstorm wind"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("torn",(sort(unique(thindata$EVTYPE))))] , "tornado"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("tropical storm",(sort(unique(thindata$EVTYPE))))] , "tropical storm"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("volcanic ash",(sort(unique(thindata$EVTYPE))))] , "volcanic ash"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("waters",(sort(unique(thindata$EVTYPE))))] , "waterspout"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep("wild",(sort(unique(thindata$EVTYPE))))] , "wildfire"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*winter*storm*"),(sort(unique(thindata$EVTYPE))))] , "winter storm"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*winter*weather*"),(sort(unique(thindata$EVTYPE))))] , "winter weather"))
thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE %in% sort(unique(thindata$EVTYPE))[grep(glob2rx("*winter*mix*"),(sort(unique(thindata$EVTYPE))))] , "winter weather"))

thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , (EVTYPE %in% permitted) == FALSE , NA ))



##  now we're doing a thing with the aggregation

propdamage <- aggregate( ADJUSTED ~ EVTYPE , data = thindata , FUN = sum )
topdamage <- arrange( propdamage , desc(ADJUSTED) )[1:5,]
topdamage <- mutate( topdamage , ADJUSTED = ADJUSTED / 1e9 )
quickplot(
    x = EVTYPE,
    y = ADJUSTED ,
    data = topdamage ,
    color = EVTYPE ,
    ylim = c(0, 150),
    xlab = "Event Type" ,
    ylab = "Property Damage (millions of US Dollars)" ,
    main = "Top Five Extreme Weather Events for Property Damage"
) + geom_bar(stat = "identity")


injury <- aggregate( INJURIES ~ EVTYPE , data = thindata , FUN = sum )
topinjury <- arrange( injury , desc(INJURIES) )[1:5,]
topinjury <- mutate( topinjury , INJURIES = INJURIES / 1e3 )

quickplot(
    x = EVTYPE,
    y = INJURIES ,
    data = topinjury ,
    color = EVTYPE ,
    ylim = c(0, 100),
    xlab = "Event Type" ,
    ylab = "Thousands of Human Injuries" ,
    main = "Top Five Extreme Weather Events for Human Injuries"
) + geom_bar(stat = "identity")

fatality <- aggregate( FATALITIES ~ EVTYPE , data = thindata , FUN = sum )
topfatality <- arrange( fatality , desc(FATALITIES) )[1:5,]
topfatality <- mutate( topfatality , FATALITIES = FATALITIES / 1e3 )

quickplot(
    x = EVTYPE,
    y = FATALITIES ,
    data = topfatality ,
    color = EVTYPE ,
    ylim = c(0, 6),
    xlab = "Event Type" ,
    ylab = "Thousands of Human Fatalities" ,
    main = "Top Five Extreme Weather Events for Human Fatalities"
) + geom_bar(stat = "identity")
