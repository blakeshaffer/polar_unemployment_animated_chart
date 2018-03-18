getCANSIM2<-function(x) {
  url<-paste("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/0",x,"-eng.zip",sep="")
  url
  temp<-tempfile()
  download.file(url,temp)
  unzip(temp,paste("0",x,"-eng.csv",sep=""))
  data<-read.csv(paste("0",x,"-eng.csv",sep=""),stringsAsFactors=FALSE)
  data$Value<-as.numeric(as.character(data$Value)) # Convert factors to numeric
  return(data)
}

## Function to format dates
convertdate<-function(x) {
  x$year <- substr(x$Ref_Date, 1, 4)
  x$month <- substr(x$Ref_Date, 6, 7)
  x$date<-paste(x$year,x$month,"01",sep="-")
  x$date <- as.Date(x$date, "%Y-%m-%d")
  temp<-x
}

# Function to rename provinces
renameprov<-function(x) {
  x$GEO[x$GEO=="Canada"]<-"CAN"
  x$GEO[x$GEO=="British Columbia"]<-"BC"
  x$GEO[x$GEO=="Alberta"]<-"AB"
  x$GEO[x$GEO=="Saskatchewan"]<-"SK"
  x$GEO[x$GEO=="Manitoba"]<-"MB"
  x$GEO[x$GEO=="Ontario"]<-"ON"
  x$GEO[x$GEO=="Quebec"]<-"QC"
  x$GEO[x$GEO=="New Brunswick"]<-"NB"
  x$GEO[x$GEO=="Nova Scotia"]<-"NS"
  x$GEO[x$GEO=="Prince Edward Island"]<-"PE"
  x$GEO[x$GEO=="Newfoundland and Labrador"]<-"NL"
  x$GEO[x$GEO=="Northwest Territories"]<-"NT"
  x$GEO[x$GEO=="Northwest Territories including Nunavut"]<-"NTNU"
  x$GEO[x$GEO=="Nunavut"]<-"NU"
  x$GEO[x$GEO=="Yukon"]<-"YT"
  temp<-x
}

# Function to rename provinces
renameCMA<-function(x) {
  x$GEO[x$GEO=="St. John's, Newfoundland and Labrador"]<-"St. John's, NL"
  x$GEO[x$GEO=="Halifax, Nova Scotia"]<-"Halifax, NS"
  x$GEO[x$GEO=="Moncton, New Brunswick"]<-"Moncton, NB"
  x$GEO[x$GEO=="Saint John, New Brunswick"]<-"Saint John, NB"
  x$GEO[x$GEO=="Saguenay, Quebec"]<-"Saguenay, QC"
  x$GEO[x$GEO=="Qu\xe9bec, Quebec"]<-"Quebec, QC"
  x$GEO[x$GEO=="Sherbrooke, Quebec"]<-"Sherbrooke, QC"
  x$GEO[x$GEO=="Trois-Rivi\xe8res, Quebec"]<-"Trois-Rivieres, QC"
  x$GEO[x$GEO=="Montr\xe9al, Quebec"]<-"Montreal, QC"
  x$GEO[x$GEO=="Ottawa-Gatineau, Ontario/Quebec"]<-NA
  x$GEO[x$GEO=="Ottawa-Gatineau, Quebec part, Ontario/Quebec"]<-"Gatineau, QC"
  x$GEO[x$GEO=="Ottawa-Gatineau, Ontario part, Ontario/Quebec"]<-"Ottawa, ON"
  x$GEO[x$GEO=="Kingston, Ontario"]<-"Kingston, ON"
  x$GEO[x$GEO=="Peterborough, Ontario"]<-"Peterborough, ON"
  x$GEO[x$GEO=="Oshawa, Ontario"]<-"Oshawa, ON"
  x$GEO[x$GEO=="Toronto, Ontario"]<-"Toronto, ON"
  x$GEO[x$GEO=="Hamilton, Ontario"]<-"Hamilton, ON"
  x$GEO[x$GEO=="St. Catharines-Niagara, Ontario"]<-"St. Catharines, ON"
  x$GEO[x$GEO=="Kitchener-Cambridge-Waterloo, Ontario"]<-"Waterloo, ON"
  x$GEO[x$GEO=="Brantford, Ontario"]<-"Brantford, ON"
  x$GEO[x$GEO=="Guelph, Ontario"]<-"Guelph, ON"
  x$GEO[x$GEO=="London, Ontario"]<-"London, ON"
  x$GEO[x$GEO=="Windsor, Ontario"]<-"Windsor, ON"
  x$GEO[x$GEO=="Barrie, Ontario"]<-"Barrie, ON"
  x$GEO[x$GEO=="Greater Sudbury, Ontario"]<-"Greater Sudbury, ON"
  x$GEO[x$GEO=="Thunder Bay, Ontario"]<-"Thunder Bay, ON"
  x$GEO[x$GEO=="Winnipeg, Manitoba"]<-"Winnipeg, MB"
  x$GEO[x$GEO=="Regina, Saskatchewan"]<-"Regina, SK"
  x$GEO[x$GEO=="Saskatoon, Saskatchewan"]<-"Saskatoon, SK"
  x$GEO[x$GEO=="Calgary, Alberta"]<-"Calgary, AB"
  x$GEO[x$GEO=="Edmonton, Alberta"]<-"Edmonton, AB"
  x$GEO[x$GEO=="Kelowna, British Columbia"]<-"Kelowna, BC"
  x$GEO[x$GEO=="Abbotsford-Mission, British Columbia"]<-"Abbotsford-Mission, BC"
  x$GEO[x$GEO=="Vancouver, British Columbia"]<-"Vancouver, BC"
  x$GEO[x$GEO=="Victoria, British Columbia"]<-"Victoria, BC"
  temp<-x
}

## Function to drop unnecessary columns and factorize provs
cleanCANSIM<-function(x) {
  x$Ref_Date <- NULL
  x$Vector <- NULL
  x$Coordinate <- NULL
  temp<-x
}

provlist<-c("BC","AB","SK","MB","ON","QC","NB","PE","NS","NL")

CMAlist<-c("St. John's, NL","Halifax, NS","Moncton, NB","Saint John, NB","Saguenay, QC",
           "Quebec, QC","Sherbrooke, QC","Trois-Rivieres, QC","Montreal, QC",
           "Gatineau, QC","Ottawa, ON","Kingston, ON","Peterborough, ON","Oshawa, ON",
           "Toronto, ON","Hamilton, ON","St. Catharines, ON","Waterloo, ON",
           "Brantford, ON","Guelph, ON","London, ON","Windsor, ON","Barrie, ON","Greater Sudbury, ON",
           "Thunder Bay, ON","Winnipeg, MB","Regina, SK","Saskatoon, SK","Calgary, AB","Edmonton, AB",
           "Kelowna, BC","Abbotsford-Mission, BC","Vancouver, BC","Victoria, BC"
)

