iata_to_icao <- function(loc){
	# loc <- "GRU"
  airports <- read.delim("airport_data/airports_world.txt", sep ="\t")
  for (i in loc){
	idx <- substring(airports$IATA,2,4) == i
	loc_icao <- substring(airports$ICAO[idx],2,5)
	return(loc_icao)
  }
}