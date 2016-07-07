#### First Steps:
setwd("/Users/patryk/Box Sync/coursera/course-work/5--reproducible-research/assignment-2")
#### Obscure that in the final docs...

################################################################################
################################################################################
####  This is my go-to initializer bitz:
####    * see what packages are installed
####    * enumerate what packs are needed
####    * install what isn't there already
####    * load all of the above

get.installed.package.list <- function() {
    package_frame <- installed.packages()
    package_list <- package_frame[, 1]
    return(unique(package_list))
}
old.package.list <- get.installed.package.list()
new.package.list <-
    c( "plyr",
       "dplyr",
       "ggplot2" ,
       "readr" ,
       "tidyr" ,
       "stringr" ,
       "lubridate" )
for (i in new.package.list) {
    if (i %in% old.package.list == FALSE) {
        install.packages(i , dependencies = TRUE)
    }   
    require(i , character.only = TRUE)
}
rm( i )

################################################################################
################################################################################
####  This is where we download and then load the data. See if we've already 
####  downloaded, if not, download. Read in using 'readr()' by Hadley, so 
####  "read_csv()" instead of the typical "read.csv()" -- underscores != dots... 

if(file.exists("StormData.csv.bz2")==FALSE) {
    download.file(
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" ,
        destfile = "StormData.csv.bz2"
    )
}

stormdata <- read_csv( "stormdata.csv.bz2" )

################################################################################
################################################################################
####  Take a look at the data:

head(stormdata) ; str(stormdata) ; names(stormdata)
unique(stormdata$EVTYPE)
length(unique(stormdata$EVTYPE))

################################################################################
################################################################################
####  typos abound. let's cut them down a bit...

stormdata$EVTYPE <- str_to_lower(stormdata$EVTYPE)
unique(stormdata$EVTYPE)
length(unique(stormdata$EVTYPE))


################################################################################
################################################################################
####  OK, reading in data is easy. First Problem is that the dataframe has too
####  many columns. The second is that event names are wonky. Even names are
####  improved by converting to all lowercase w/ str_to_lower() above. Let's 
####  pull a tighter set of columns before we go any further.

thindata <- select(stormdata , EVTYPE , FATALITIES:PROPDMGEXP )
thindata
unique(thindata$PROPDMGEXP)
length( unique( thindata$PROPDMGEXP ))

#thindata$PROPDMGEXP <- as.factor( thindata$PROPDMGEXP )

table(thindata$PROPDMGEXP)

################################################################################
################################################################################
####  Oh, Third Problem:  PROPDMGEXP entries are wonky. Let's clean that before
####  EVTYPE.  According to the NOAA "code book," the only approved entries for 
####  PROPDMGEXP are K, M, B; meaning thousands, millions, billions of dollars.
####  I'm electing to roll everything else into NA. There's no way to infer it.
thindata$PROPDMGEXP <- str_to_upper(thindata$PROPDMGEXP)
thindata <- mutate( thindata , PROPDMGEXP = replace( PROPDMGEXP , 
                             ( PROPDMGEXP %in% c("K" , "M" , "B")) == F , NA ))
####  It's really unfortunate that like half our data is already NA about this,
####  but we can only analyse the data that's comprehensible. Some of this is 
####  just poorly recorded.
thindata <- na.omit( thindata )
####  and they're gone...

################################################################################
################################################################################
####  Now let's get back to the second problem. This is much like the third prob
####  but with many more layers.  "permitted" -- "The only events permitted in
####  Storm Data are listed in Table 1 of Section 2.1.1" of  NATIONAL WEATHER
####  SERVICE INSTRUCTION 10-16.
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
sort(unique(thindata$EVTYPE) , decreasing = T )

thindata <- mutate(thindata , EVTYPE = replace(EVTYPE , EVTYPE == 
    "blizzard/winter storm" , "blizzard"))

thindata <- mutate(thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c( 
    "coastal surge","coastal flooding/erosion","coastal flooding",
    "coastal flood","coastal erosion","coastal  flooding/erosion",
    "coastal storm") , "coastal flood" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == "cold" , 
    "cold/wind chill" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == "fog" , 
    "dense fog" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
    "heat wave drought" , "drought" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
    "dust devil waterspout" , "dust devil" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
    "dust storm/high winds" , "dust storm" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
    "extreme heat" , "excessive heat" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "extended cold","extreme cold","extreme wind chill","extreme windchill") , 
    "extreme cold/wind chill" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "flash flood - heavy rain","flash flood from ice jams",
    "flash flood landslides","flash flood/","flash flood/ street",
    "flash flood/flood","flash flood/landslide","flash flooding",
    "flash flooding/flood","flash flooding/thunderstorm wi","flash floods",
    "flood flash","flood/flash","flood/flash flood","flood/flash/flood",
    "flood/flashflood") , "flash flood" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "flood & heavy rain","flooding","floods","flood/river flood",
    "heavy rain and flood","heavy rains/flooding") , "flood" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "frost","frost\\freeze","freeze"), "frost/freeze" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "hail 0.75","hail 100","hail 175","hail 275","hail 450","hail 75",
    "hail damage","hail/wind","hail/winds","hailstorm") , "hail" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
    "heat wave" , "heat" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "heavy rain/severe weather","heavy rain/small stream urban",
    "heavy rain/snow","heavy rains" ), "heavy rain" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c( 
    "heavy snowpack","heavy snow/winter storm","heavy snow/wind","heavy snow/squalls",
    "heavy snow/ice","heavy snow/high winds & flood","heavy snow/freezing rain",
    "heavy snow/blizzard/avalanche","heavy snow/blizzard","heavy snow-squalls",
    "heavy snow squalls","heavy snow shower","heavy snow and strong winds" )
    , "heavy snow" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "heavy rain/high surf","heavy surf","heavy surf coastal flooding",
    "heavy surf/high surf","high surf","high surf advisory","rough surf" )
    , "high surf" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "winter storm high winds","high winds/snow","high winds/heavy rain",
    "high winds/cold","high winds/coastal flood","high winds/",
    "high winds heavy rains","high winds","high wind/seas",
    "high wind/heavy snow","high wind/blizzard","high wind damage",
    "high wind and seas","high wind 48","high wind (g40)","high wind",
    "high  winds" ), "high wind" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "hurricane/typhoon","hurricane-generated swells","hurricane opal/high winds",
    "hurricane opal","hurricane gordon","hurricane felix","hurricane erin",
    "hurricane emily","hurricane","typhoon" ), "hurricane" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "snow/ice storm","snow/ice","snow/ ice","snow and ice storm","snow and ice",
    "ice/strong winds","ice storm" ), "ice storm" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "lake-effect snow","lake effect snow","heavy lake snow" )
    , "lake-effect snow" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "lakeshore flood","lake flood" ) , "lakeshore flood" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "heavy rain/lightning","lightning/heavy rain","lightning thunderstorm winds",
    "lightning fire","lightning and heavy rain","lightning  wauseon","lightning",
    "lighting" ), "lightning" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
                               "marine tstm wind", "marine thunderstorm wind" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
                                                "rip currents", "rip current" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "snow/sleet/freezing rain","snow/sleet","sleet/ice storm",
    "freezing rain/sleet" ), "sleet" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "storm surge/tide","storm surge","tidal flooding" ), "storm surge/tide" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "strong winds","strong wind"
)          , "strong wind" ))

### this one is trickier. there are a lot of 'thunderstorm' wind values with a
### lot of different spellings and abreviations.
thunderset <- unique(c(grep( 'thun' , sort(unique(thindata$EVTYPE) , 
    decreasing = T )) , grep( 'tstm' , sort(unique(thindata$EVTYPE) , 
    decreasing = T ))))
thunderset <- thunderset[ -c( 8,36,37,39,40,46,64 )]
thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% 
    thunderset, "thunderstorm wind" ))

tornadoset <- grep( 'torn' , sort(unique(thindata$EVTYPE) , decreasing = T ) )
thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% 
    tornadoset  , "tornado" ))

tropicset <- grep( 'tropical storm', sort((unique(thindata$EVTYPE) )))
thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% 
    tropicset , "tropical storm" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE ==
    "volcanic ashfall" , "volcanic ash" ))

waterspoutset <-grep( 'waterspout', sort((unique(thindata$EVTYPE) )))
thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in%
    waterspoutset, "waterspout" ))

wildfireset <- grep( 'wild' , sort(unique(thindata$EVTYPE)))
thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in%
                                                     wildfireset , "wildfire" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE == 
    "winter storms" , "winter storm" ))

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% c(
    "winter weather mix","winter weather/mix" ) , "winter weather" ))

### that cleans up all of the data EVTYPEs that can be reasonably corrected.
### now we push all of the ones that weren't resolved to the permitted list
### into 'other'

thindata <- mutate( thindata , EVTYPE = replace( EVTYPE , EVTYPE %in% 
    permitted == FALSE , "other" ))
sort(unique(thindata$EVTYPE))

### Success! That thins the event types down to that which was prescribed by 
### NOAA. 

### Next we have to apply the K,M,B property damage multipliers to the property 
### damage numbers so that they are comparible.

names(thindata) <- c("EVTYPE" , "FATALITIES" , "INJURIES" , "PROPDMG" , 
                     "MAGNITUDE" )
thindata <- mutate( thindata , MAGNITUDE = replace( MAGNITUDE , MAGNITUDE == "K" , 1e3))
thindata <- mutate( thindata , MAGNITUDE = replace( MAGNITUDE , MAGNITUDE == "M" , 1e6))
thindata <- mutate( thindata , MAGNITUDE = replace( MAGNITUDE , MAGNITUDE == "B" , 1e9))
unique(thindata$MAGNITUDE)
thindata$MAGNITUDE <- as.numeric(thindata$MAGNITUDE)
thindata <- mutate( thindata , ADJUSTED = PROPDMG * MAGNITUDE )

