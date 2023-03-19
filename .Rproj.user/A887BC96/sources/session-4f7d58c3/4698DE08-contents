##### PARSING COMPLETLY RAW TEXT #####

#' This function is used with text parsing to grab a header that is up above
#' an entry and associate it with all entries below it
#' @param base.text.loc the locations (row numbers) of the entries
#' @param header.loc the locations (row numbers) of the headers
#' @param input.vec the input text vector that contains both of the above
#' @return a data frame
#' @export
grab_section_headers <- function(base_text_loc, header_loc, input_vec){
  output_vec <- vector(length = length(input_vec))
  for(i in base_text_loc){
    maxless <- suppressWarnings(max(header_loc[header_loc <= i]))
    output_vec[[i]] <- input_vec[maxless]
  }
  return(output_vec)
}



##### WRAPPERS AROUND READING IN FILES #####

#' wrapper for readxl::read_xlsx() that forces all cols to character
#' which also avoids the very stupid erroneous autoguessing errors stemming from
#' fields which have too many NA's in their records
#' @param xlsx_path a path to an xlsx file
#' @export
read_xlsx_char <- function(xlsx_path){
  readxl::read_xlsx(xlsx_path, col_types = "text")
}



#' wrapper for readr::read_csv that forces all cols to character
#' which also avoids the very stupid erroneous auto-guessing errors stemming from
#' fields which have too many NA's in their records
#' @param csv_path a path to a csv file
#' @export
read_csv_char <- function(csv_path){
  df <- readr::read_csv(csv_path, col_types = cols(.default = "c"))
  return(df)
}



#' Read Collection Code tsv
#' This function reads in a tsv in the standard
#' Cal Academy Collection data format with correct
#' data types assigned
#' @param collectionDBFilePath a tsv file in standard Cal Academy Collection Code Format
#' @return  a data frame file in standard Cal Academy Collection Code Format
#' @export
read_cc_db <- function(collectionDBFilePath){
  db_cc <- readr::read_tsv(collectionDBFilePath,
                           col_types = cols(CollectionCode = col_character(),
                                            BiogeographicRegion = col_character(),
                                            Country = col_character(),
                                            Adm1 = col_character(),
                                            Adm2 = col_character(),
                                            Region = col_character(),
                                            Locale = col_character(),
                                            LocalityNotes = col_character(),
                                            Lat = col_character(),
                                            Lon = col_character(),
                                            LatLonMaxError = col_character(),
                                            Datum = col_character(),
                                            Elevation = col_character(),
                                            ElevationMaxError = col_character(),
                                            DateCollectedStart = col_date(format = ""),
                                            DateCollectedEnd = col_date(format = ""),
                                            Habitat = col_character(),
                                            Microhabitat = col_character(),
                                            Method = col_character(),
                                            Medium = col_character(),
                                            CollectedBy = col_character(),
                                            CollectionNotes = col_character(),
                                            LocalityCode = col_character()
                           )
  )
  return(db_cc)
}



#' Read Specimen Code tsv
#' This function reads in a tsv in the standard
#' Cal Academy Specimen data format with correct
#' data types assigned
#' @param specimenDBFilePath a tsv file in standard Cal Academy Specimen Code Format
#' @return a data frame file in standard Cal Academy Specimen Code Format
#' @export
read_sc_db <- function(specimenDBFilePath){
  db_sc <- readr::read_tsv(specimenDBFilePath,
                           col_types = cols(SpecimenCode = col_character(),
                                            Class = col_character(),
                                            Order = col_character(),
                                            Family = col_character(),
                                            Subfamily = col_character(),
                                            Genus = col_character(),
                                            Species = col_character(),
                                            LifeStageSex = col_character(),
                                            DeterminedBy = col_character(),
                                            DateDetermined = col_date(format = ""),
                                            TypeStatus = col_character(),
                                            SpecimenNotes = col_character(),
                                            DNANotes = col_character(),
                                            LocatedAt = col_character(),
                                            OwnedBy = col_character(),
                                            BiogeographicRegion = col_character(),
                                            Country = col_character(),
                                            Adm1 = col_character(),
                                            Adm2 = col_character(),
                                            Region = col_character(),
                                            Locale = col_character(),
                                            LocalityNotes = col_character(),
                                            Lat = col_character(),
                                            Lon = col_character(),
                                            LatLonMaxError = col_character(),
                                            Datum = col_character(),
                                            Elevation = col_character(),
                                            ElevationMaxError = col_character(),
                                            DateCollectedStart = col_date(format = ""),
                                            DateCollectedEnd = col_date(format = ""),
                                            Habitat = col_character(),
                                            Microhabitat = col_character(),
                                            Method = col_character(),
                                            Medium = col_character(),
                                            CollectedBy = col_character(),
                                            CollectionNotes = col_character(),
                                            LocalityCode = col_character()
                           )
  )
  return(db_sc)
}



#' Read Label Inbox tsv
#' This function reads in a tsv in the standard
#' Cal Academy Specimen data format with correct
#' data types assigned
#' @param specimenDBFilePath a tsv file in standard Cal Academy Specimen Code Format
#' @return a data frame file in standard Cal Academy Specimen Code Format
#' @export
read_label_inbox <- function(specimenDBFilePath){
  label_inbox <- readr::read_tsv(specimenDBFilePath,
                                 col_types = cols(Code = col_character(),
                                                  Class = col_character(),
                                                  Order = col_character(),
                                                  Family = col_character(),
                                                  Subfamily = col_character(),
                                                  Genus = col_character(),
                                                  Species = col_character(),
                                                  LifeStageSex = col_character(),
                                                  DeterminedBy = col_character(),
                                                  DateDetermined = col_date(format = ""),
                                                  TypeStatus = col_character(),
                                                  SpecimenNotes = col_character(),
                                                  DNANotes = col_character(),
                                                  LocatedAt = col_character(),
                                                  OwnedBy = col_character(),
                                                  BiogeographicRegion = col_character(),
                                                  Country = col_character(),
                                                  Adm1 = col_character(),
                                                  Adm2 = col_character(),
                                                  Region = col_character(),
                                                  Locale = col_character(),
                                                  LocalityNotes = col_character(),
                                                  Lat = col_character(),
                                                  Lon = col_character(),
                                                  LatLonMaxError = col_character(),
                                                  Datum = col_character(),
                                                  Elevation = col_character(),
                                                  ElevationMaxError = col_character(),
                                                  DateCollectedStart = col_date(format = ""),
                                                  DateCollectedEnd = col_date(format = ""),
                                                  Habitat = col_character(),
                                                  Microhabitat = col_character(),
                                                  Method = col_character(),
                                                  Medium = col_character(),
                                                  CollectedBy = col_character(),
                                                  CollectionNotes = col_character(),
                                                  LocalityCode = col_character()
                                 )
  )
  return(label_inbox)
}



##### MANAGING DATABASES #####

#' pseudo-relational database update for CASENT collection and specimen code databases
#' PSEUDO RELATIONAL DATABASE: OVERWRITE DISCREPANCIES IN SPECIMEN CODE DATABASE db_cc -> db_sc
#' @param db_sc Specimen code database in CASENT format
#' @param db_cc Collection code database in CASENT format
#' @return an updated specimen database
#' @export
update_sc_from_cc <- function(db_sc, db_cc){

  # check if db_sc is in unparsed state so it can be return as such
  is_unparsed <- all(!names(db_sc) %in% "BaseCode")

  # make sure base and sub codes are parsed
  db_sc %<>% parse_codes()
  db_cc %<>% parse_codes()

  # pull db_sc specific columns that won't be overwritten
  db_sc_untouched <- db_sc %>% select(SpecimenCode, BaseCode, SubCode, Class, Order, Family, Subfamily, Genus, Species, LifeStageSex, DeterminedBy,
                                      DateDetermined, TypeStatus, SpecimenNotes, LocatedAt, OwnedBy, DNANotes, Medium)

  # pull out columns from db_cc that will overwrite those from db_sc
  db_cc_overwrite <- db_cc %>% select(BaseCode, BiogeographicRegion, Country, Adm1, Adm2, Region, Locale, LocalityNotes, Lat, Lon, LatLonMaxError,
                                      Datum, Elevation, ElevationMaxError, DateCollectedStart, DateCollectedEnd, Habitat, Microhabitat,
                                      Method, CollectedBy, CollectionNotes, LocalityCode)
  db_sc_new <- db_sc_untouched %>%
    left_join(db_cc_overwrite, by = 'BaseCode')
  # restore column order
  db_sc_new <- bind_rows(db_sc[0,], db_sc_new)
  db_sc_new <- arrange(db_sc_new, SpecimenCode)

  # return db_sc to deparsed state if it was originally so
  if(is_unparsed){
    db_sc_new %<>% deparse_codes()
    db_cc %<>% deparse_codes()
  }

  return(db_sc_new)
}



#' Locality Code Lookup
#' This function reads locality codes and imports locality names
#' from the locality code dictionary for collection code records.
#' It ignores records that have no locality code entered.
#' @param df a data frame in standard Cal Academy Collection Code Format
#' @param loc_key a data frame file in standard Cal Academy Collection Code Format
#' @return a data frame with updated locality information
#' @export
add_loc_info <- function(df, loc_key){
  locC_is_na <- df[is.na(df$LocalityCode),]
  locC_is_not_na <- df[!is.na(df$LocalityCode),]

  # only removing columns when they exist makes this function agnostic to different database styles (i.e. critter club and insect collection)
  # these columns will be added back in with the join
  if('Region' %in% colnames(locC_is_not_na)){locC_is_not_na$Region <- NULL}
  if('Locale' %in% colnames(locC_is_not_na)){locC_is_not_na$Locale <- NULL}
  if('Lat' %in% colnames(locC_is_not_na)){locC_is_not_na$Lat <- NULL}
  if('Lon' %in% colnames(locC_is_not_na)){locC_is_not_na$Lon <- NULL}
  if('LatLonMaxError_meters' %in% colnames(locC_is_not_na)){locC_is_not_na$LatLonMaxError_meters <- NULL}
  if('LatLonMaxError' %in% colnames(locC_is_not_na)){locC_is_not_na$LatLonMaxError <- NULL}
  if('Habitat' %in% colnames(df)){locC_is_not_na$Habitat <- NULL}

  # convert data types to character to match loc.key format
  locC_is_not_na$LocalityCode <- as_character(locC_is_not_na$LocalityCode)


  # create the join
  locC_is_not_na <- left_join(locC_is_not_na, loc_key, by = 'LocalityCode')

  # put data frame back together
  df <- bind_rows(locC_is_na, locC_is_not_na)


  # sort by collection code (if that column exists)
  if('CollectionCode' %in% colnames(df)){df <- arrange(df, CollectionCode)}
  return(df)
}



#' Splits up an insect collection dataframe's (collection or specimen dataframe) code column
#' into constituent parts: base code number and subcode (if present).
#' used to match specimen codes with their parent collection codes efficiently
#' @param df Either a collection code or speciment code database in CASENT format
#' @return an updated database
#' @export
parse_codes <- function(df){
  # check if data frame is a collection code dataframe
  if(any(names(df) %in% "CollectionCode")){
  df$BaseCode <- as.numeric(str_match(df$CollectionCode, '\\w+#(\\d+)')[,2])
  df$SubCode <- as.numeric(str_match(df$CollectionCode, '\\w+#(\\d+)-(\\d+)')[,3])
  }
  # check if data frame is a specimen code dataframe
  if(any(names(df) %in% "SpecimenCode")){
  df$BaseCode <- as.numeric(str_match(df$SpecimenCode, '\\w+#(\\d+)')[,2])
  df$SubCode <- as.numeric(str_match(df$SpecimenCode, '\\w+#(\\d+)-(\\d+)')[,3])
  }
  # check if data frame is a label_inbox dataframe
  if(any(names(df) %in% "Code")){
    df$BaseCode <- as.numeric(str_match(df$Code, '\\w+#(\\d+)')[,2])
    df$SubCode <- as.numeric(str_match(df$Code, '\\w+#(\\d+)-(\\d+)')[,3])
  }
  return(df)
}



#' removes the base and subcode columns that the function parse codes adds
#' @param df Either a collection code or speciment code database in CASENT format
#' @return an updated database
#' @export
deparse_codes <- function(df){
  df$BaseCode <- NULL
  df$SubCode <- NULL
  return(df)
}



#' Checks for missing data in key collection fields
#' This function takes a standard critter club data frame
#' and checks for blanks in missing fields
#' @param df standard Cal Academy Insect Dataframe for writing insect labels
#' @export
check_empty <- function(df){
  cols_to_check <- df[,c("Country", "Adm1", "Adm2", "Lat", "Lon", "Locale", "DateCollectedStart", "Elevation", "CollectedBy")]
  # send warnings for missing label data
  cat(paste0("checking for missing critical data in labels\n"))
  for(i in names(cols_to_check)){
    len <- sum((is.na(cols_to_check[[i]])), na.rm = TRUE)
    if(len > 0) print(paste0("warning! the ", i, " column is missing ", len, " values"))
  }
  if(any(is.na(cols_to_check))){ # give user the choice to stop if any of the above reported fields are missing
    answer <- readline(paste0("fields are empty! are you sure you want to continue? (y/n)"))
    if(answer!="y" & answer!="y"){
      cat(paste0("program terminated- fill in missing fields and rerun script"))
      stop()
    }
  }
  cat(paste0("continuing...\n"))
}



#' this function filters out records based on time difference between observations
#'it assumes the data is sorted first by a unique id vector and then by ascending date/time
#' @param obs observation database in standard critter club format
#' @export
filter_time_diff <- function(obs, ind = 1) {
  ind_next <- first(which(difftime(obs, obs[ind], units = "hours") > 1))
  if (is.na(ind_next))
    return(ind)
  else
    return(c(ind, filter_time_diff(obs, ind_next)))
}



#' Checks that all genera listed in a set of observations occurs in the specified key or checklist
#' @param obs observation databse in standard critter club format
#' @param key key or checklist in standard critter club format
#' @export
find_bad_G <- function(obs, key){
  noGMatch <- obs[!obs$Genus %in% key$Genus,]
  if(nrow(noGMatch) > 0){
    cat(paste0('\nWarning there are genera in observations that are not in your key or checklist!\n'))
    cat(paste0("The genus ", noGMatch$Genus, " from observation data can't be found\n"))
  }
}



#' This function is used to generate a binomial and trinomial column based on
#' Pre-existing genus and species columns
#' @param df a data frame of occurrence records in Symbiota format
#' @return a data frame with binomial and trinomial columns
#' @export
bi_tri <- function(df){
  # function that takes a bio database and combines genus and specificEpithet into binomial and trinomial columns and returns the database
  if("genus" %in% names(df) & "specificEpithet" %in% names(df) & "infraspecificEpithet" %in% names(df)){
    # remove binomial and trinomial columns if they already exist
    if('binomial' %in% names(df)){
      df %<>% select(-binomial)
    }
    if('trinomial' %in% names(df)){
      df %<>% select(-trinomial)
    }
    # only fill values for binomial and trinomial when appropriate data is available
    df$binomial <- ifelse(!is.na(df$specificEpithet) & !is.na(df$genus), paste(df$genus, df$specificEpithet), NA)
    df$trinomial <- ifelse(!is.na(df$infraspecificEpithet), paste(df$genus, df$specificEpithet, df$infraspecificEpithet), paste(df$genus, df$specificEpithet))

    # properly relocate columns
    df %<>% relocate(binomial, .after = specificEpithet)
    df %<>% relocate(trinomial, .after = binomial)
    return(df)
  } else{ cat(paste0("genus, specificEpithet, or infraspecificEpithet columns are missing!"))
  }
}

#'function that takes biological records that only have a scientificName column
#' and parses it and adds Linnaean columns: kingdom, phylum, class, order,
#' family, genus...
#' @param df a data frame of occurrence records in Symbiota format
#' @return a data frame with expanded Linnaean columns
#' @export
sci_name_parser <- function(df){
  if("scientificName" %in% names(df)){
    # add in blank higher taxonomy scaffold for inputting later
    df$kingdom <- NA
    df$phylum <- NA
    df$class <- NA
    df$order <- NA
    df$family <- NA

    # erase "sp." or "sp" with a space in front of it as these are not needed
    df$scientificName <- gsub(' sp\\.?$', "", df$scientificName)

    # separate dataframe based on records which have spaces (and thus have binomials or trinomials),
    # and those that do not
    dfSpace <- df %>% filter(grepl(" ", scientificName))
    dfNoSpace <- df %>% filter(!grepl(" ", scientificName))

    # for those with spaces seperate out the scientficName field by spaces
    dfSpace %<>% separate(scientificName,
                          c("genus", "specificEpithet", "infraspecificEpithet"),
                          sep = ' ', extra = "drop", fill = "right"
    )

    # for those without spaces, assess what the linnean rank is and parse accordingly
    # dfNoSpace$scientificName %in% dict$family
  } else{ message("can't parse scientificName column because it is missing!")
  }
}

#' this is a custom function to fix common name capitalization nuances
#' i.e. title casing but not capitalizing after hyphens
#' @param vec a vector of common names of organisms
#' @return a vector with results
#' @export
common_name_caps <- function(vec){
  vec <- tolower(vec) # make everything lower case to start on equal footing
  vec <- gsub('^(\\w)', '\\U\\1', vec, perl=TRUE) # capitalize the first letter of the whole string
  vec <- gsub(' (\\w)', '\\U \\1', vec, perl=TRUE) # capitalize any letter that comes after a space (i.e. not after hyphens, etc.)
  return(vec)
}



#' Custom function to do table join to fill in data from common name lists
#' probably don't need this function anymore
#' i.e. title casing but not capitalizing after hyphens
#' @param df a dataframe
#' @param checklist # a checklist
#' @return a data frame with results
#' @export
lookup_common_name <-function(df, checklist){
  template <- df[0,] # create template to preserve column order
  dfCommon <- df %>% filter(!is.na(commonName)) # save entries that already have a common name entered
  dfNoCommon <- df %>% filter(is.na(commonName))
  dfNoCommon <- left_join(select(dfNoCommon, -commonName), select(checklist, binomial, commonName), by = 'binomial')
  bind_rows(dfCommon, dfNoCommon, template)
  return(df)
}





##### GIS PROCESSES #####

#'function which geoencodes records based on date and time stamps with gpx files
#' @param records a data frame of occurence records that contains decimalLatitude and decimalLongitude columns
#' @param timeCoords a csv of coordinates with time stamps usually from gps unit
#' @param timezone timezone in which to interpret the biological records for matching to timeCoords (which are in UTC)
#' @return a dataframe with filled in lat lon values
#' @export
extract_coords <- function(records, timeCoords, timezone){
  # READ IN AND PROCESS MANUAL IMPORT FORM FOR MATCHING TO COORDINATE STAMPS
  # this will return TRUE if any timestamp has any extra information beyond hh:mm:ss
  # which will check that nothing has been corrupted
  if(grepl('^\\d\\d:\\d\\d:\\d\\d.+$', records$time) %>% any()){
    stop('something is wrong with time stamp format')
  }
  records$dateTime <- as.POSIXct(paste(records$eventDate, records$time), format="%Y-%m-%d %H:%M")
  records_GPS <- records %>% filter(!is.na(decimalLatitude)) # filter out anything with lat/lon or Locality Code
  records_noGPS <- records %>% filter(is.na(decimalLatitude)) # filter out anything without lat/lon or Locality Code
  if(nrow(records_noGPS) < 1){
    cat(paste0("All records have coordinates already! This script is not needed!\n"))
  }
  # split no GPS records into those with and without time
  records_time <- filter(records_noGPS, !is.na(time))
  records_notime <- filter(records_noGPS, is.na(time))

  # convert from time zone to UTC
  records_time$dateTime <- records_time$dateTime - hours(timezone)
  # force time zone to UTC without letting R jack with the actual time
  records_time$dateTime <- lubridate::force_tz(records_time$dateTime, 'UTC')
  # select only columns needed for query
  records_time_query <- records_time %>% select(decimalLatitude, decimalLongitude, dateTime) %>% arrange(dateTime)
  # Change decimalLatitude/decimalLongitude to integer so data frames will combine
  records_time_query$decimalLatitude <- as.numeric(records_time_query$decimalLatitude)
  records_time_query$decimalLongitude <- as.numeric(records_time_query$decimalLongitude)
  timeCoords$decimalLatitude <- as.numeric(timeCoords$decimalLatitude)
  timeCoords$decimalLongitude <- as.numeric(timeCoords$decimalLongitude)

  # create a type for track points vs our records
  timeCoords$type <- "track point"
  records_time_query$type <- "collection record"
  joined_table <- bind_rows(records_time_query, timeCoords)

  # create a seamless date/time arranged table of records and track points
  joined_table <- joined_table %>% arrange(dateTime)
  # create a running ID number for the table
  joined_table$ID <- 1:nrow(joined_table)
  # create a vector of just the ID locations of just our records in the table for looping through
  cc_id_vec <- joined_table %>% filter(type == 'collection record')
  cc_id_vec <- as.vector(cc_id_vec[['ID']])

  # create a version of joined.table without records for use in loop below
  joined_table_no_rec <- filter(joined_table, type == 'track point')

  # create loop to find the closest time value in the gps track for each record
  counter = 0
  for(i in cc_id_vec){
    query_row <- filter(joined_table, ID == i)
    # create a new sort table devoid of non-focal collection records
    new_sort <- bind_rows(query_row, joined_table_no_rec)
    new_sort <- arrange(new_sort, dateTime)
    # get row number of focal collection code in new sort table
    query_row_num <- which(new_sort$ID == i)
    query_row <- new_sort[query_row_num,]
    # get rows above and below focal record (closest in time)
    row_above <- new_sort[query_row_num - 1,]
    row_below <- new_sort[query_row_num + 1,]
    above_diff <- query_row$dateTime - row_above$dateTime
    below_diff <- query_row$dateTime - row_below$dateTime

    # check if either a row above or row below match cannot be found in track file
    if(is.na(row_above$dateTime) | is.na(row_below$dateTime)){
      cat(paste0("Warning, check collection records and track files, some collection records are not surrounded by gpx time stamps"))
    }
    # find the smaller of the two absolute values of the time differences
    closest_row <- if(abs(above_diff) < abs(below_diff)){
      as_tibble(row_above)
    } else{
      as_tibble(row_below)
    }
    # convert to POSIXct class for manipulating time differences
    query_row$dateTime <- as.POSIXct(query_row$dateTime)
    closest_row$dateTime <- as.POSIXct(closest_row$dateTime)
    time_diff <- difftime(query_row$dateTime, closest_row$dateTime, units = 'mins')
    time_diff_abs <- abs(time_diff)

    # warn if collection records is too separated in time from closest track point
    counter = 0
    if(time_diff_abs < 15){
      # Input correct decimalLatitude and decimalLongitude into records with date and time stamps if timestamp has a match in gps track within 15 min
      records_time$decimalLatitude[records_time$dateTime == query_row$dateTime] <- closest_row$decimalLatitude
      records_time$decimalLongitude[records_time$dateTime == query_row$dateTime] <- closest_row$decimalLongitude
      # cat(paste0('Collection record at timestamp ', query_row$dateTime, ' is ', round(time_diff, 1),
      #            ' minutes separated from the nearest GPS trackpoint\n'))
    } else{cat(paste0('WARNING: collection record at timestamp ', query_row$dateTime, ' is ', round(time_diff, 0),
                      ' minutes separated from the nearest GPS trackpoint, please verify correct location.\n'))
    }
  }
  # check if any new coordinates have effectively been assigned before continuing
  if(any(!is.na(records_time$decimalLatitude))){
    # Put database back together
    rec_new <- bind_rows(records_GPS, records_time, records_notime)
    rec_new %<>% arrange(eventDate, time)
    rec_new$dateTime <- NULL
    cat(paste0('\n', length(na.omit(rec_new$decimalLatitude)) - length(na.omit(records$decimalLatitude)),
               ' records were updated with coordinates based on gpx timestamps\n'))
    cat(paste0("WARNING: ", sum(is.na(rec_new$decimalLatitude)),
               ' records are still missing coordinates\n'))
    return(rec_new)
  } else{
    cat(paste0('WARNING: no new records were updated from gpx files \n'))
    return(records)
  }
}



#' Reads in gpx files recursively from a designated path and assembles and formats one exhuative dataframe with just location
#' and time stamps sorted in order of occurence
#' @param path a path to parent folder where gpx files occur
#' @return a data frame of coords and time stamps for all gpx files specified
#' @export
munge_gpx <- function(path){
  # grab all gpx files recursively from the specified parent folder
  gpx_files <- list.files(path = path, pattern = "\\.gpx$", recursive = T, full_names=TRUE)

  # run lapply to read in all gpx files and htmlTreeParse to pull out timestamp info and genarate a list of individual dataframes
  # corresponding to each gpx file.
  ldf <- lapply(gpx_files, function(file){
    pfile <- XML::htmlTreeParse(file = file, error = function(...) {}, useInternalNodes = T)
    elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
    times <- XML::xpathSApply(pfile, path = "//trkpt/time", xmlValue)
    coords <- XML::xpathSApply(pfile, path = "//trkpt", xmlAttrs)
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    df <- data.frame(Lat = lats, Lon = lons, Elevation = elevations, DateTime = times)
    rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
    return(df)
  }
  )
  # combine all dataframes to one big master coordinate dataframe of all gpx trails
  all_coords <- data.table::rbindlist(ldf) # I used this rather than a dplyr solution because it's faster
  all_coords$DateTime %<>% lubridate::ymd_hms() # fix date formatting
  all_coords %<>% dplyr::arrange(DateTime)
  return(all_coords)
}


#' Populate elevation using the geonames server based on
#' decimalLatitutde and decimalLongitude columns in a dataframe
#' # I obtained my username (which is 'roverso') from here to validate the elevation lookup request
#' http://www.geonames.org/export/web-services.html
#' @param df a database in SYMBIOTA format
#' @return a dataframe with rounded lat lon values
#' @export
find_elevation <- function(df){
  dfCoords <- df %>% select(decimalLatitude, decimalLongitude)
  dfCoords$decimalLatitude <- as.numeric(dfCoords$decimalLatitude)
  dfCoords$decimalLongitude <- as.numeric(dfCoords$decimalLongitude)
  elevationQuery <- rgbif::elevation(dfCoords, username = 'roverso')
  df$minimumElevationInMeters <- elevationQuery$elevation_geonames
  df$minimumElevationInMeters <- as.character(df$minimumElevationInMeters)
}



#' This function attaches desired data from a shapefile to a Symbiota formatted
#' data frame.
#' @param df a Symbiota formatted database
#' @param poly a simple feature formatted shapefile of polygons
#' @param fieldName The name of the single field in the shapefile you want to extract
#' @param newName User-defined new name for the newly attached field in the dataframe
#' @export
#' @return returns a database with desired column from the shapefile attached
rev_geocode <- function(df, poly, fieldName, newName){
  df %<>% dplyr::filter(!is.na(decimalLatitude)) # remove any rows without coordinates
  # convert data frame to simple feature (sf) format (note that 4326 defines WGS84)
  dfPoints <- sf::st_as_sf(df, coords = c(x = "decimalLongitude", y = "decimalLatitude"), crs = 4326)
  # For points that fall within polygons, adds attributes, NOTE! retains all points if left=TRUE, otherwise uses inner_join
  dfPoints <- sf::st_join(dfPoints, left = TRUE, poly[fieldName]) # join points
  # check and see if there is already data in the new name user defined to protect it
  if(newName %in% names(df)){
    results <- pull(dfPoints, fieldName)
    df[newName] <- ifelse(is.na(df[,newName]), results, pull(df[newName]))
  } else{
    # if user-defined new column doesn't exist just mutate a new column
    # pull the focal data from the points object and bind to original
    # df. !! and := notation is needed for dplyr to assign col name with a variable rather than a string
    df %<>% dplyr::mutate(!!newName := dplyr::pull(dfPoints, fieldName))
  }
}


#' reverse geocode function. This function takes a standard Cal Academy
#' database and uses the lat lon columns to add additional reverse
#' geocoded information if it is not already filled in.
#' @param df a standard Cal Academy Insect Database format for insect labels
#' @export
#' @return returns a database with reverse geocoded fields
rev_geocode_insect <- function(df){
  cat(paste0("Starting reverse geocoding\n"))

  df$Lat_bk <- df$Lat  # make backup of Lat Lon formatting
  df$Lon_bk <- df$Lon

  # CONVERT LAT/LON TO NUMERIC
  df$Lat <- as.numeric(df$Lat)
  df$Lon <- as.numeric(df$Lon)

  df_coords <- df[!is.na(df$Lat),] # remove rows that have no coordinates
  df_no_coords <- df[is.na(df$Lat),] # remove rows that have no coordinates (no further processing on these records)

  # turn my data frame into a Spatial Point object and project to WGS84
  coords <- as.data.frame(cbind(Lon = df_coords$Lon,Lat = df_coords$Lat))
  points <- coords
  sp::coordinates(points) <- ~Lon + Lat
  df_spdf <- sp::SpatialPointsDataFrame(points, df_coords)
  sp::proj4string(df_spdf) <- sp::CRS("+proj=longlat +ellps=WGS84")
  df_spdf <- sp::spTransform(df_spdf, sp::CRS("+proj=longlat +ellps=WGS84"))

  # read in boundary shape files and project to WGS84
  # define paths
  shape_file_path <- "./data_supplement/shape_files"
  shape_file_path_mex <- "./data_supplement/shape_files/Mexico"
  shape_file_path_arg <- "./data_supplement/shape_files/Argentina"
  # read in
  countries <- rgdal::readOGR(dsn=shape_file_path, layer="ne_110m_admin_0_countries")
  countries <- sp::spTransform(countries, sp::CRS("+proj=longlat +ellps=WGS84"))
  states <- rgdal::readOGR(dsn=shape_file_path, layer="ne_110m_admin_1_states_provinces_shp")
  states <- sp::spTransform(states, sp::CRS("+proj=longlat +ellps=WGS84"))
  counties <- rgdal::readOGR(dsn=shape_file_path, layer="cb_2013_us_county_500k")
  counties <- sp::spTransform(counties, sp::CRS("+proj=longlat +ellps=WGS84"))
  natparks <- rgdal::readOGR(dsn=shape_file_path, layer="ne_10m_parks_and_protected_lands_area")
  natparks <- sp::spTransform(natparks, sp::CRS("+proj=longlat +ellps=WGS84"))
  natforests <- rgdal::readOGR(dsn=shape_file_path, layer="S_USA.NFSLandUnit")
  natforests <- sp::spTransform(natforests, sp::CRS("+proj=longlat +ellps=WGS84"))
  # cities <- rgdal::readOGR(dsn=shape_file_path, layer="cities_dtl")
  # cities <- sp::spTransform(cities, sp::CRS("+proj=longlat +ellps=WGS84"))

  # bring data from shapefile polygons to my spatial points dataframe where they intersect
  # i.e. intersect points in newly converted spatial dataframe with polygons and add polygon IDS to pts@data.
  # the mutate_if commands are necessary to de-factorize the queried data otherwise factor levels are pasted below
  # into my data frame rather than the names of regions
  countries_query <- sp::over(df_spdf, countries[,c("ABBREV","CONTINENT")]) %>% mutate_if(is.factor, as.character)
  states_query <- sp::over(df_spdf, states[,"name"]) %>% mutate_if(is.factor, as.character)
  counties_query <- sp::over(df_spdf, counties[,"NAME"]) %>% mutate_if(is.factor, as.character)
  natparks_query <- sp::over(df_spdf, natparks[,"unit_name"]) %>% mutate_if(is.factor, as.character)
  natforests_query <- sp::over(df_spdf, natforests[,"NFSLANDU_2"]) %>% mutate_if(is.factor, as.character)
  # cities_query <- sp::over(df_spdf, cities[,"NAME"]) %>% mutate_if(is.factor, as.character)
  # overwrite fields with reverse geocoded data only if reverse geocode data is not blank
  df_coords$BiogeographicRegion <- ifelse(!is.na(countries_query$CONTINENT), countries_query$CONTINENT, df_coords$BiogeographicRegion)
  df_coords$Country <- ifelse(!is.na(countries_query$ABBREV), countries_query$ABBREV, df_coords$Country)
  df_coords$Adm1 <- ifelse(!is.na(states_query$name), states_query$name, df_coords$Adm1)
  df_coords$Adm2 <- ifelse(!is.na(counties_query$NAME), counties_query$NAME, df_coords$Adm2)
  # cities <- cities_query$NAME
  # Sequentially fill in any blanks in the REGION Column from two different sources prioritizing the first source
  df_coords$Region <- ifelse(is.na(df_coords$Region), natparks_query$unit_name, df_coords$Region)
  df_coords$Region <- ifelse(is.na(df_coords$Region), natforests_query$NFSLANDU_2, df_coords$Region)

  # Load and add in Mexico data only if there are still blanks from the above US shapefiles to save time
  if(any(is.na(df_coords$Adm1))){
    # read in Mexico
    mex_adm1 <- rgdal::readOGR(dsn=shape_file_path_mex, layer="dest_2010gw")
    mex_adm1 <- sp::spTransform(mex_adm1, sp::CRS("+proj=longlat +ellps=WGS84"))
    mex_adm2 <- rgdal::readOGR(dsn=shape_file_path_mex, layer="Muni_2012gw")
    mex_adm2 <- sp::spTransform(mex_adm2, sp::CRS("+proj=longlat +ellps=WGS84"))
    # bring data from shapefile polygons to my spatial points dataframe where they intersect for Mexico
    mex_adm1_query <- sp::over(df_spdf, mex_adm1[,"ENTIDAD"]) %>% mutate_if(is.factor, as.character)
    mex_adm2_query <- sp::over(df_spdf, mex_adm2[,"NOM_MUN"]) %>% mutate_if(is.factor, as.character)
    # overwrite fields with reverse geocoded data only if reverse geocode data is not blank for Mexico
    df_coords$Adm1 <- ifelse(!is.na(mex_adm1_query$ENTIDAD), mex_adm1_query$ENTIDAD, df_coords$Adm1)
    df_coords$Adm2 <- ifelse(!is.na(mex_adm2_query$NOM_MUN), mex_adm2_query$NOM_MUN, df_coords$Adm2)
  }

  # Load and add in Argentina data only if there are still blanks from the above shapefiles to save time
  if(any(is.na(df_coords$Adm1))){
    # read in Argentina
    arg_adm1 <- rgdal::readOGR(dsn=shape_file_path_arg, layer="ARG_adm1")
    arg_adm1 <- sp::spTransform(arg_adm1, sp::CRS("+proj=longlat +ellps=WGS84"))
    arg_adm2 <- rgdal::readOGR(dsn=shape_file_path_arg, layer="ARG_adm2")
    arg_adm2 <- sp::spTransform(arg_adm2, sp::CRS("+proj=longlat +ellps=WGS84"))
    # bring data from shapefile polygons to my spatial points dataframe where they intersect for Argentina
    arg_adm1_query <- sp::over(df_spdf, arg_adm1[,"NAME_1"]) %>% mutate_if(is.factor, as.character)
    arg_adm2_query <- sp::over(df_spdf, arg_adm2[,"NAME_2"]) %>% mutate_if(is.factor, as.character)
    # overwrite fields with reverse geocoded data only if reverse geocode data is not blank for Argentina
    df_coords$Adm1 <- ifelse(!is.na(arg_adm1_query$NAME_1), arg_adm1_query$NAME_1, df_coords$Adm1)
    df_coords$Adm2 <- ifelse(!is.na(arg_adm2_query$NAME_2), arg_adm2_query$NAME_2, df_coords$Adm2)
  }

  # OBTAIN ELEVATION FROM GBIF FROM LAT/LON with Google API
  api_key <- "AIzaSyBdXLWomxOy7h2F_q-PZPmvJ5tZ115N3X0"

  latlon <- df_coords[,colnames(df_coords) == "Lat" | colnames(df_coords) == "Lon"]
  colnames(latlon)[colnames(latlon) == "Lat"] <- "decimalLatitude"
  colnames(latlon)[colnames(latlon) == "Lon"] <- "decimalLongitude"
  elevation_query <- rgbif::elevation(input = latlon, key = api_key)
  df_coords$Elevation <- formatC(elevation_query$elevation, 0, format = "f")


  # CONVERT LAT/LON TO NUMERIC
  df_coords$Lat <- df_coords$Lat_bk
  df_coords$Lon <- df_coords$Lon_bk

  # Fix Reverse Geocode Formating
  df_coords$Country <- gsub("U\\.S\\.A.", "USA", df_coords$Country)
  df_coords$Country <- gsub("Can\\.", "Canada", df_coords$Country)
  df_coords$Country <- gsub("Mex\\.", "Mexico", df_coords$Country)
  df_coords$Country <- gsub("Baja CA", "Baja California Norte", df_coords$Country)
  df_coords$Country <- gsub("Arg\\.", "Argentina", df_coords$Country)
  df_coords$Region <- gsub("\\sNP", " National Park", df_coords$Region)
  df_coords$Region <- gsub("\\sNS", " National Seashore", df_coords$Region)
  df_coords$Adm2 <- gsub("Do\xf1a Ana", "Doña Ana", df_coords$Adm2) # Fix typo in shapefiles
  df_coords$Adm1 <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(df_coords$Adm1), perl=TRUE) # convert any all caps to title case

  df_coords$Adm1 <- gsub2(state_name, state_abb, df_coords$Adm1) # Sub state abbreviations for names

  df <- dplyr::bind_rows(df_coords, df_no_coords)

  df$Lat_bk <- NULL # remove uneeded columns
  df$Lon_bk <- NULL # remove uneeded columns

  cat(paste0("Ending reverse geocoding\n\n"))
  return(df)
}


#' Rounds lat lon values based on sensible precision and manage formats
#' based on accuracy information in the "coordinateUncertaintyInMeters" field
#' @param df a database in SYMBIOTA format
#' @return a dataframe with rounded lat lon values
#' @export
round_coords_cc <- function(df){
  # Make sure Lat/Lon associated columns are numeric
  df$decimalLatitude <- as.numeric(df$decimalLatitude)
  df$decimalLongitude <- as.numeric(df$decimalLongitude)
  df$coordinateUncertaintyInMeters <- as.numeric(df$coordinateUncertaintyInMeters)

  # Temporarily remove records with "NA" or no numbers in the coordinateUncertaintyInMeters field so they don't get processed
  dfnoNumUncertainty <- df[grepl("^([^0-9]*)$", df$coordinateUncertaintyInMeters, perl = TRUE),]
  dfNAUncertainty <- df[is.na(df$coordinateUncertaintyInMeters),]
  df <- df[grepl("\\d", df$coordinateUncertaintyInMeters, perl = TRUE),]
  # remove records without coords temporarily
  dfNoCoords <- df %>% filter(is.na(decimalLatitude))
  dfCoords <- df %>% filter(!is.na(decimalLatitude))

  # ROUND LAT/LON TO APPROPRIATE VALUES BASED ON ACCURACY
  dfCoords['roundNum'] <- NA
  dfCoords$roundNum[dfCoords$coordinateUncertaintyInMeters >= 1000] <- 2
  dfCoords$roundNum[dfCoords$coordinateUncertaintyInMeters >= 100 & dfCoords$coordinateUncertaintyInMeters < 1000] <- 3
  dfCoords$roundNum[dfCoords$coordinateUncertaintyInMeters >= 10 & dfCoords$coordinateUncertaintyInMeters < 100] <- 4
  dfCoords$roundNum[dfCoords$coordinateUncertaintyInMeters < 10] <- 5
  dfCoords$roundNum <- as.numeric(dfCoords$roundNum)

  # Overwrite rounded Lat/Lon results with original values
  results <- NA
  for(i in 1:nrow(dfCoords)){
    results[[i]] <- formatC(dfCoords$decimalLatitude[i], dfCoords$roundNum[i], format = "f")
  }
  dfCoords$decimalLatitude <- results

  results <- NA
  for(i in 1:nrow(dfCoords)){
    results[[i]] <- formatC(dfCoords$decimalLongitude[i], dfCoords$roundNum[i], format = "f")
  }
  dfCoords$decimalLongitude <- results

  # Delete columns that aren't needed
  dfCoords$roundNum <- NULL
  dfCoords$decimalLatitude <- as.numeric(dfCoords$decimalLatitude)
  dfCoords$decimalLongitude <- as.numeric(dfCoords$decimalLongitude)

  # Add back records that were removed pre-processing above and then re-sort database
  df <- bind_rows(dfCoords, dfNoCoords, dfNAUncertainty, dfnoNumUncertainty)

  # Format as character values so no funny business happens to Lat/Lon
  df$decimalLatitude <- as.character(df$decimalLatitude)
  df$decimalLongitude <- as.character(df$decimalLongitude)

  # Sort whatever column is necessary before returning output since I cut and pasted the df so much
  if("SpecimenCode" %in% colnames(df)){
    df <- df[with(df, order(SpecimenCode)),] # sort cc.new;
  } else{ if("CollectionCode" %in% colnames(df)){
    df <- df[with(df, order(CollectionCode)),] # sort cc.new
  }
  }
  return(df)
}



#' rounds lat lon values based on sensible precision and manage formats
#' based on accuracy information field records
#' @param df a database in CASENT format
#' @return a dataframe with rounded lat lon values
#' @export
round_coords_insect <- function(df){
  print("Starting lat lon rounder based on GIS accuracy")
  # Temporarily remove records with "NA" or no numbers in the LatLonMaxError field so they don't get processed
  df_noNum <- df[grepl("^([^0-9]*)$", df$LatLonMaxError, perl = TRUE),]
  df_na <- df[is.na(df$LatLonMaxError),]
  df <- df[grepl("\\d", df$LatLonMaxError, perl = TRUE),]

  # Extract the number from LatLonMaxError String
  df <- mutate(df, LatLonMaxError_NUM = str_extract(df$LatLonMaxError, "[\\d.]+"))

  # Make sure Lat/Lon associated columns are numeric
  df$Lat <- as.numeric(df$Lat)
  df$Lon <- as.numeric(df$Lon)
  df$LatLonMaxError_NUM <- as.numeric(df$LatLonMaxError_NUM)

  # ROUND LAT/LON TO APPROPRIATE VALUES BASED ON ACCURACY
  df['roundNum'] <- NA
  df$roundNum[df$LatLonMaxError_NUM >= 1000] <- 2
  df$roundNum[df$LatLonMaxError_NUM >= 100 & df$LatLonMaxError_NUM < 1000] <- 3
  df$roundNum[df$LatLonMaxError_NUM >= 10 & df$LatLonMaxError_NUM < 100] <- 4
  df$roundNum[df$LatLonMaxError_NUM < 10] <- 5

  df$roundNum <- as.numeric(df$roundNum)

  # Overwrite rounded Lat/Lon results with original values
  results <- NA
  for(i in 1:nrow(df)){
    results[[i]] <- formatC(df$Lat[i], df$roundNum[i], format = "f")
  }
  df$Lat <- results

  results <- NA
  for(i in 1:nrow(df)){
    results[[i]] <- formatC(df$Lon[i], df$roundNum[i], format = "f")
  }
  df$Lon <- results

  # Format as character values so no funny business happens to Lat/Lon
  df$Lat <- as.character(df$Lat)
  df$Lon <- as.character(df$Lon)

  # Delete columns that aren't needed
  df$roundNum <- NULL
  df$LatLonMaxError_NUM <- NULL

  # Add back records that were removed pre-processing above and then re-sort database
  df <- rbind(df, df_noNum, df_na)

  # Sort whatever column is necessary before returning ouput since I cut and pasted the df so much
  if("SpecimenCode" %in% colnames(df)){
    df <- df[with(df, order(SpecimenCode)),] # sort cc.new;
  } else{ if("CollectionCode" %in% colnames(df)){
    df <- df[with(df, order(CollectionCode)),] # sort cc.new
  }
  }
  return(df)
  print("Ending LatLonSigFig")
}






##### FUNCTIONS FOR WORKING WITH GBIF API #####
#' function that makes manual edits based on a csv key before records are queried
#' at gbif
#' @param df a data frame
#' @param bandAid a data frame
#' @return a data frame
#' @export
pre_query_patchr <- function(df, bandAid){
  bandAid %<>% select(scientificName, bandAidSciName)
  df %>%
    left_join(bandAid, 'scientificName') %>%
    mutate(
      scientificName = case_when(
        !is.na(bandAidSciName) ~ bandAidSciName,
        TRUE ~ scientificName
      )
    ) %>%
    select(-bandAidSciName)
}



#' Flattens the nested structure of a rgbif backbone_name() query
#'and streamlines other inconsistencies in data structure
#' @param df a df with a column that contains nested query response
#' @param resultColumn name of result column with response from rgbif::backbone_name()
#' @return a data frame
#' @export
flatten_rgbif <- function(df, resultColumn){
  if(!is_empty(df[[resultColumn]])){ # check that nested structure is not empty
    df %<>% bind_cols(df[[resultColumn]])
    # if there are no species-level results returned—this creates a blank species
    # column so code doesn't break below
    if(!'species' %in% names(df)){
      df$species <- NA
      df$speciesKey <- NA
    }
    df[[resultColumn]] <- NULL # remove nested results
    return(df)
  }
}




#' This function matches a Symbiota formatted data frame of biological occurrences to
#' a local gbif dictionary created by gbif_dictionary_builder.R and updates taxonomic
#' names accordingly while warning the user about names that cannot be matched
#' @param df a dataframe
#' @param dict
#' @return a data frame
#' @export
taxo_updatr <- function(df, dict){
  # join focal data frame to dictionary
  df %<>% rename(originalSciName = scientificName) %>%
    left_join(dict, by = 'originalSciName')
  # fill in gbif english common names only if we don't already have one
  df$commonName <- ifelse(is.na(df$commonName), df$commonNameEng, df$commonName)
  df %<>% select(-commonNameEng)
  # check and warn how many scientificNames were updated
  notChangedCount <- (df$scientificName == df$originalSciName) %>% sum(na.rm = TRUE)
  changedCount <- nrow(df) - notChangedCount
  message(changedCount, ' of ',  nrow(df), ' observations records were modified by the GBIF backbone taxonomy')
  # check and warn how many scientificNames could not be found in the dictionary
  noMatch <- df %>% filter(is.na(scientificName))
  message(nrow(noMatch), ' records were unable to be found in the dictionary and will be removed')
  # reorder columns—note that the contains() function can be used if the column may or may not exist
  df %<>%
    relocate(scientificName, originalSciName, rank, kingdom, phylum, class, order,
             family, contains('subfamily'), genus, contains('binomial')) %>%
    relocate(commonNameSpa, .after = commonName)
  return(df)
}



#' Wrapper function to query rgbif for common names and append them to biological
#' occurrence records. Functions takes a data frame of records and attaches
#' common names using rgif in English and Spanish. Note that common names have to
#' be queried by scientific name key numbers and not from strings of names.
#' For convenience, the usageKey field returned by gbif is the numerical key
#' referring to the lowest ranked taxonomic name for convenience
#' @param df a data frame
#' @return a data frame
#' @export
common_namer <- function(df){
  if(length(df$usageKey) > 0){
    # if the English common name column already exists, filter only entries that aren't filled in to save time
    # and temporarily remove those columns so they can be re-added from the gbif query later
    if('commonNameEng' %in% names(df)){
      commonNameYes <- df %>% filter(!is.na(commonNameEng))
      df %<>% filter(is.na(commonNameEng))
      df %<>% select(-commonNameEng, -commonNameSpa)
    }
    # remove records that do not have a usage key anyway
    sciNameNo <- df %>% filter(is.na(usageKey))
    df %<>% filter(!is.na(usageKey))
    # create vector of usage keys to query common names
    keyVec <- df %>% select(usageKey) %>% pull()
    # vectorize the rgbif::name_usage function so I can use it on a list of names
    v_name_usage <- Vectorize(rgbif::name_usage, SIMPLIFY = FALSE)
    # query gbif for common names
    vernacularQuery <- v_name_usage(keyVec, data = 'vernacularNames')

    # create a template before initiating the loop
    commonNames <- vernacularQuery[[1]]$data[0,]
    # create a for loop to extract common names from the nested data structure returned by name_usage()
    for(i in 1:length(vernacularQuery)){
      commonNames <- vernacularQuery[[i]]$data %>% bind_rows(commonNames)
    }

    # check if taxonNames column exists before proceeding (if it doesn't there was no data returned from gbif)
    if("taxonKey" %in% names(commonNames)){
      commonNames %<>% select(taxonKey, vernacularName, language, source, any_of("preferred")) # select columns I need
      commonNames %<>% filter(language == 'eng' | language == 'spa') # filter languages I want


      # sort by priority so the slice command that remove the first row for each group will get the best common name entry
      if("preferred" %in% names(commonNames)){ # check if preferred column exists in output and use if it is
        commonNames$preferred %<>% as.character() %>% replace_na('none') # replace NA in preferred field so I can sort and prioritize
        commonNames$preferred <- factor(commonNames$preferred, levels = c('TRUE', 'none', 'FALSE' ))
        commonNames %<>% arrange(taxonKey, language, preferred)
      } else{
        commonNames %<>% arrange(taxonKey, language)
      }

      # slice records so there is one common name for each language for each record
      commonNames %<>% group_by(taxonKey, language) %>% slice(1) %>% ungroup() %>%
        select(-source)

      # check if preferred column exists in output and remove it if it is
      commonNames %<>% select(-any_of("preferred"))

      # split data frames by language and clean for joining to ocurrence records
      commonNamesEng <- commonNames %>% filter(language == 'eng') %>% select(-language) %>%
        rename(usageKey = taxonKey, commonNameEng = vernacularName)
      commonNamesSpa <- commonNames %>% filter(language == 'spa') %>% select(-language) %>%
        rename(usageKey = taxonKey, commonNameSpa = vernacularName)
      # join common name query results to where they match to the occurrence records
      df %<>% mutate_all(as.character)
      commonNamesEng %<>% mutate_all(as.character)
      commonNamesSpa %<>% mutate_all(as.character)

      df %<>% left_join(commonNamesEng, by = 'usageKey') %>%
        left_join(commonNamesSpa, by = 'usageKey')
      rm(keyVec, vernacularQuery, commonNames, commonNamesEng, commonNamesSpa)
      # standardize the casing of common names which are inconsistently formatted in GBIF
      # English
      df$commonNameEng %<>% str_to_title() # first convert names to title case to streamline
      df$commonNameEng <- gsub('-(\\w)', '-\\L\\1', df$commonNameEng, perl = TRUE) # lower the case after hyphen
      # Spanish
      df$commonNameSpa %<>% str_to_title() # first convert names to title case to streamline
      df$commonNameSpa <- gsub('-(\\w)', '-\\L\\1', df$commonNameSpa, perl = TRUE) # lower the case after hyphen
      df$commonNameSpa <- gsub(' De ', ' de ', df$commonNameSpa, perl = TRUE) # lower the case of the word 'de' in spanish
      df$commonNameSpa <- gsub(' Del ', ' del ', df$commonNameSpa, perl = TRUE) # lower the case of the word 'de' in spanish
    }
    # recombine rows that may have been removed
    if(exists('commonNameYes')){df %<>% bind_rows(commonNameYes)}
    if(exists('sciNameNo')){df %<>% bind_rows(sciNameNo)}
    return(df)
  }
}



##### WORKING WITH TAXONOMY HEIRARCHY #####

#' Function that removes higher linnean rank names from the scientificName column
#' if the are already represented by lower ranked names, for example, removing
#' 'Lepidoptera' if 'Danaus plexippus' already exists in the same column.
#' @param df a dataframe
#' @param rankList a list of linnean rank names
#' @return a data frame
#' @export
higher_rank_remover <- function(df, rankList){
  rankLength <- rankList %>% length
  runLength <- rankLength - 1

  for(i in 1:runLength){
    # split out data set based on current level
    dfSubset <- filter(df, rank == rankList[i])
    focalCols <- rankList[(i + 1):rankLength]
    dfSubset %<>% select(focalCols)

    # this little loop inside a loop—probably can be written more efficiently but it basically just collapses
    # the whole data frame into a vector of unique values so they can be used to filter the database
    removeList <- c() # initialize list to store unneeded taxon records
    for(j in 1:(length(focalCols))){
      removeList <- c(removeList, sort(unlist(dfSubset[j], use.names = F)))
    }
    removeList %<>% sort() %>% unique() # collapse to distinct values
    remove <- df %>% filter(scientificName %in% removeList)
    df %<>% filter(!scientificName %in% removeList)
    return(df)
  }
}



#' Function to sort biological occurrence records by linnean ranks
#' @param df a data frame
#' @return a data frame
#' @export
linnean_releveler <- function(df){
  # forcats::relevel() function is important to reorder factor levels because base R will erase factors
  # that are not explicitly stated and I don't want to lose new taxonomic levels that arrive in future data
  # just because I don't state their order
  kingdomSort <- c("Animalia", "Plantae", "Fungi", "Protozoa", "incertae sedis")
  phylumSort <- c("Chordata", "Arthropoda", "Mollusca", "Ascomycota", "Basidiomycota", "Tracheophyta", "Rhodophyta")
  classSort <- c("Mammalia", "Aves", "Reptilia", "Amphibia", "Actinopterygii", "Elasmobranchii", "Cephalaspidomorphi",
                 "Insecta", "Arachnida", "Malacostraca", "Magnoliopsida",  "Pinopsida", "Gnetopsida", "Liliopsida",
                 "Polypodiopsida", "Lycopodiopsida")

  # remove entries from the sort list that are not in the focal data frame to avoid warnings of missing factors
  kingdomSort <- kingdomSort[kingdomSort %in% as.character(unique(df$kingdom))]
  phylumSort <- phylumSort[phylumSort %in% as.character(unique(df$phylum))]
  classSort <- classSort[classSort %in% as.character(unique(df$class))]

  # convert Linnean taxonomic ranks to factors and relevel factor entries according to sort lists
  df$kingdom %<>% as_factor() %>% fct_relevel(kingdomSort)
  df$phylum %<>% as_factor() %>% fct_relevel(phylumSort)
  df$class %<>% as_factor() %>% fct_relevel(classSort)
  df$order %<>% as_factor() # for now don't have any sorting priorities for order
  # sorting by family (only have data for birds)
  ebirdKey <- read_csv_char(pathEbirdTakonKey) # read in ebird key to get family taxonomy sorting
  ebirdKey$famOrder %<>% as.integer()
  familySort <- ebirdKey %>% arrange(famOrder) %>% distinct(family) %>% filter(!is.na(family)) %>% pull()
  rm(ebirdKey)
  familySort <- familySort[familySort %in% df$family] # pull out only ebird taxa that are in the dfionary
  df$family %<>% as_factor() %>% fct_relevel(familySort) # make family a sorted factor like other ranks above
  return(df)
}



#' Given a dataframe and taxonomic rank vector—function (sorted highest to lowest) this function
#'will mutate the column 'lowestRank' which will contain the lowest non-NA taxon value among the
#'columns specified. Note that the syms() function is required to convert strings to variables
#' @param df a data frame
#' @param rankVec a data frame
#' @return a data frame
#' @export
lowTaxonFinder <- function(df, rankVec){
  # this saves of copy of the data frame preserve class types so they
  # don't get mangled with functions below that need columns converted to character
  df2 <- df
  # use the above function rowwise in a dataframe (must be character only)
  df2 %<>% rowwise() %>% mutate(across(everything(), as.character)) %>%
    mutate(lowestRank = first_finder(do.call(rbind, rlang::syms(rev(rankVec)))))
  # find the highest rank for relocating the results column
  relocateSpot <- rankVec[1]
  df %<>% cbind(lowestRank = df2$lowestRank) %>% relocate(lowestRank, .before = all_of(relocateSpot))
}





##### OTHER FUNCTIONS #####

#' Paste function that doesn't carry over NA values
#' @return a vector with results
#' @export
pasteSansNA <- function(..., sep = ", ") {
  L <- list(...)
  L <- lapply(L, function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^", sep, "|", sep, "$)"), "",
             gsub(paste0(sep, sep), sep,
                  do.call(paste, c(L,list(sep=sep)))
             )
  )
  is.na(ret) <- ret == ""
  return(ret)
}



#' Function that finds the first non-blank entry in a vector for use with dplyr::rowwise()
#' The suppressWarnings is for warnings when their are no elements in the vector
#' @param x a vector
#' @return the value of the firt non-blank entry in the vector
#' @export
first_finder <- function(x){x[suppressWarnings(min(which(!is.na(x))))]}


#' Given a dataframe and vector of column names ranked in the order of highest to lowest priority,
#' this function will mutate the column 'first' which will contain the first occuring non-NA value among the
#' columns specified. Note that the syms() function is required to convert strings to variables
#' @param df a data frame
#' @param rank_vec a data frame
#' @return a data frame
#' @export
na_skipper <- function(df, rank_vec){

  # this saves of copy of the data frame preserve class types so they
  # don't get mangled with functions below that need columns converted to character
  df2 <- df

  # use the above function rowwise in a dataframe (must be character only)
  df2 %<>% rowwise() %>% mutate(across(everything(), as.character)) %>%
    mutate(first_unblank = first_finder(do.call(rbind, rlang::syms(rank_vec))))

  # find the highest rank for relocating the results column
  relocateSpot <- rank_vec[1]

  df %<>% cbind(first_unblank = df2$first_unblank) %>% relocate(first_unblank, .before = all_of(relocateSpot))

  return(df)

}



#' Convert to standard insect dates for labels
#' @param date_vector_start a vector of start dates
#' @param date_vector_end a vector of end dates
#' @return a dataframe column of date or date range that collection took place
#' @export
ento_dates <- function(date_vector_start, date_vector_end){
  # format with lubridate package
  ifelse(any(!is.na(date_vector_end)),
         df <- data.frame(DateCollectedStart = lubridate::ymd(date_vector_start), DateCollectedEnd = lubridate::ymd(date_vector_end)),
         df <- data.frame(DateCollectedStart = lubridate::ymd(date_vector_start), DateCollectedEnd = NA)
  )


  # parse out year, month, day with lubridate
  df$years <- year(df$DateCollectedStart)
  df$months <- month(df$DateCollectedStart)
  df$days <- day(df$DateCollectedStart)
  df$yeare <- year(df$DateCollectedEnd)
  df$monthe <- month(df$DateCollectedEnd)
  df$daye <- day(df$DateCollectedEnd)

  # substitute a list of month numbers for a list of roman numerals
  from <- c('10','11','12','1','2','3','4','5','6','7','8','9')
  to <- c('x','xi','xii','i','ii','iii','iv','v','vi', 'vii','viii','ix')
  df$months <- gsub2(from, to, df$months)
  df$monthe <- gsub2(from, to, df$monthe)

  # records with start but no end date
  df_noe <- df[!is.na(df$DateCollectedStart) & is.na(df$DateCollectedEnd),]
  if(nrow(df_noe) > 0){
    df_noe$DatesCollected <- paste0(df_noe$days,".",df_noe$months,".",df_noe$years)
  }

  # records with start and end date
  df_e <- df[!is.na(df$DateCollectedEnd),]
  if(nrow(df_e) > 0){
    df_e$DatesCollected <- ifelse(df_e$months == df_e$monthe,
                                  paste0(df_e$days,"–",df_e$daye,".",df_e$months,".",df_e$years),
                                  paste0(df_e$days,"–",df_e$daye,".",df_e$months,"–",df_e$monthe,".", df_e$years)
    )
  }

  # record's with no dates
  df_nod <- df[is.na(df$DateCollectedStart),]
  if(nrow(df_nod) > 0){
    df_nod$DatesCollected <- paste0("(no date)")
  }

  # combine date types back to one dataframe
  df <- rbind(df_noe, df_e, df_nod)

  # extract date vector
  return(df$DatesCollected)
}



#' this function receives collection code records in standard format
#' and splits them into specimen code records based on user input and then
#' adds them to the specimen code master database
#' @param cc.records a dataframe of collection codes
#' @param db_sc a specimen database
#' @return split specimen records
#' @export
split_cc_code <- function(cc.records, db_sc){
  template.sc <- db_sc[0,] # make template from specimen code database

  if(nrow(cc.records) > 0){ # check that collection code input has at least one row and if not, end

    scCount <- rep(NA, nrow(cc.records)) # generate blank list equaling number of needed counts
    cat(paste0("User input required for the following multi-specimen collections codes:"))

    for(i in 1:nrow(cc.records)){ # generate specimen counts in a for loop while querying user
      scCount[i] <- readline(paste0('How many PINNED specimen codes to generate for collection code: \"', cc.records$CollectionCode[i],
                                    ', ', cc.records$Region[i], ', ', cc.records$Locale[i],'\"? '))
      scCount <<- as.numeric(scCount)
    }
    cc.records <- cc.records[scCount != 0,] # remove cc.records where 0 was entered
    scCount <- scCount[scCount != 0] # remove from scCount where 0 was entered
    cc.records$SplitCount <- as.numeric(scCount)
    cc.records <- cc.records %>% select(SplitCount, everything()) # move focal columns to the front

    last.sc <- find_max_sc(ccode, db_sc) # find the last used specimen code to avoid duplication

    # if this is the first time a collection code has been split then the original code in the db_sc
    # that is not hyphenated needs to have a "-1" added onto it, and a label should be printed for it as well
    if(last.sc == 1){
      db_sc.match <- str_extract(db_sc$SpecimenCode, "\\d+") %>% as.integer() %in% ccode
      db_sc[db_sc.match,]$SpecimenCode <- paste0(db_sc[db_sc.match,]$SpecimenCode, "-1")
    }

    # expand collection code database to create new specimen code records
    cc.records.exp <- cc.records %>%
      dplyr::group_by(CollectionCode) %>%
      expand(Count = seq(1:SplitCount)) %>%
      dplyr::left_join(cc.records, by = 'CollectionCode') %>%
      dplyr::mutate(Count = Count + last.sc) %>%
      dplyr::mutate(SpecimenCodeTemp = case_when(last.sc > 0 ~ paste(CollectionCode, Count, sep = '-'),
                                                 last.sc == 0 ~ CollectionCode)) %>%
      ungroup() %>%
      dplyr::select(-CollectionCode, -SplitCount, -Count, SpecimenCodeTemp) %>%
      dplyr::rename(SpecimenCode = SpecimenCodeTemp)

    # bind duplicated collection code rows to the specimen code database template
    new.sc.records <- dplyr::bind_rows(template.sc, cc.records.exp)
    new.sc.records <- new.sc.records %>% dplyr::select(names(template.sc))
    return(new.sc.records)
  } else{cat(paste0("No collection codes found!\n"))
  }
}



#' function that fills in missing taxon information—generates a dataframe
#' with same format as original dataframe needs to either have a 'knowntaxalevel'
#' and 'taxonidinput' column, or dataframe needs to have the following columns:
#' 'class', 'order', 'family', 'subfamily', 'genus', 'species'. function outputs
#' a dataframe with same row and column order as original dataframe.
#' @param df a database in CASENT format
#' @param key # key containing taxonomic information
#' @export
fill_taxa <- function(df, key){
  cat(paste0("beginning taxa fill\n\n"))

  df_template <- df[0,] # create template to later preserve column order

  df$sort <- 1:nrow(df) # create column for restoring row order

  if('knowntaxalevel' %in% colnames(df)){ # this part of the control loop will only process field form data

    df$class <- na
    df$order <- na
    df$family <- na
    df$subfamily <- na
    df$genus <- na
    df$species <- na
    df[['genus species']] <- na

    # distribute taxonidinput to the correct taxa column based on knowntaxalevel
    for(i in 1:nrow(df)){
      if(!is.na(df$knowntaxalevel[i])){
        taxa_rank <- df$knowntaxalevel[[i]]
        df[i,taxa_rank] <- df$taxonidinput[[i]]
      }
    }

    # split up genus and species
    if(!all(is.na(df$`genus species`))){
      df$genus <- str_match(df[['genus species']], "(.+)\\s(.+)")[,2]
      df$species <- str_match(df[['genus species']], "(.+)\\s(.+)")[,3]
      df["genus species"] <- null
    }
  }

  # this part of the loop fills in taxonomic entries from the key
  # generate hierarchical keys
  sf_key <- select(key[!duplicated(key$subfamily),], -genus)
  f_key <- select(key[!duplicated(key$family),], -subfamily, -genus)
  o_key <- select(key[!duplicated(key$order),], -family, -subfamily, -genus)

  # start filling in taxa information
  has_genus <- filter(df, !is.na(genus)) # records which have genus
  a <- merge_with_order(select(has.genus, -class, -order, -family, -subfamily), key, by='genus', all.x = true, keep_order = 1)
  remainder <- filter(df, is.na(genus)) # records that don't have genus

  has_subfamily <- filter(remainder, !is.na(subfamily)) # records that have subfamily instead
  b <- merge_with_order(select(has_subfamily, -class, -order, -family), sf_key, by='subfamily', all.x = true, keep_order = 1)
  remainder <- filter(remainder, is.na(subfamily)) # records that also don't have subfamily

  has_family <- filter(remainder, !is.na(family)) # records that have family instead
  c <- merge_with_order(select(has_family, -class, -order), f_key, by='family', all.x = true, keep_order = 1)
  remainder <- filter(remainder, is.na(family)) # records that also don't have family

  has_order <- filter(remainder, !is.na(order)) # records that have order instead
  d <- merge_with_order(select(has_order, -class), o_key, by='order', all.x = true, keep_order = 1)
  remainder <- filter(remainder, is.na(order)) # records that also don't have order (all the rest)

  df_final <- bind_rows(a, b, c, d, remainder)

  # check that everything's put back together correctly
  if(nrow(df) != nrow(df_final)){
    cat('something is wrong! records are missing after filling in taxon information')
    stop()
  }


  # correct taxonomic names that have changed
  ###(fill in later)

  # return correct sort order
  df_final <- arrange(df_final, sort)

  # return correct columns in order of original inputted data frame
  df_temp <- select_(df_final, .dots = names(df_template))

  # check if input data had taxon information and if not add taxon columns
  if(!'genus' %in% names(df_template)){
    df_temp <- cbind(df_temp, select(df_final, class, order, family, subfamily, genus, species))
    df_temp <- select(df_temp, collectioncode, class, order, family, subfamily, genus, species, everything())
  }

  df_final <- df_temp
  cat(paste0("ending taxa fill\n\n"))
  return(df_final)

}



#' function that safely reads in CASENT insect database and handles dates with lubridate
#' reads in all other columns as character/text
#' @param path path to CASENT insect database in Excel format
#' @param sheet.num sheet number that database is located on
#' @return an XLS database with dates that are not messed up
#' @export
read_dates_safely <- function(path, sheet.num){
  df <- readxl::read_excel(path, sheet = sheet.num, na = c('NA', ""))
  df$CollectionCode <- as.character(df$CollectionCode)
  df$DateCollectedStart <- stringr::str_match(df$DateCollectedStart, "\\d+-\\d+-\\d+") # get rid of the time part of the date left by Excel
  df$DateCollectedEnd <- stringr::str_match(df$DateCollectedEnd, "\\d+-\\d+-\\d+") # get rid of the time part of the date left by Excel
  df$DateCollectedStart <- lubridate::ymd(df$DateCollectedStart) # Convert dates for Master Database
  df$DateCollectedEnd <- lubridate::ymd(df$DateCollectedEnd) # Convert dates for Master Database
  return(df)
}



#' function that keeps original order after merging data frames
#' @export
merge_with_order <- function(x, y, ..., sort = T, keep_order){
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")

    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }

  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()

  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}




#' function that finds miles in a string of plain English
#' and converts to km in a string
#' @param vec a vector of values in miles
#' @return a vector of values in kilometers
#' @export
mi_to_km <- function(vec){
  z <- str_match(vec, "^(.*?)(\\d+\\.?\\d+)( miles .*?)$")
  prefix <- z[,2]
  miles <- z[,3]
  suffix <- z[,4]
  km <- round(as.numeric(miles) * 1.60934, 1)
  suffix <- gsub("miles", "km", suffix)
  new <- paste3(prefix, km, suffix, sep="")
  out <- ifelse(!is.na(new), new, vec)
  return(out)
}



#' streamline word spellings for CASENT style insect database
#' @param df a database in CASENT format
#' @return an updated dataframe in CASENT format
#' @export
clean_ento_fields <- function(df){
  df$Locale <- mi_to_km(df$Locale) # change miles to km in locale
  df$CollectedBy <- "RP Overson"
  df$LocatedAt <- "RP Overson Collection"
  df$OwnedBy <- "RP Overson"
  df$Datum <- "WGS 84"
  df$Method <- gsub("black light", "UV light", df$Method)
  df$Method <- gsub("blacklight", "UV light", df$Method)
  df$Method <- gsub("hand/aspirator", "hand", df$Method)
  df$Locale <- gsub("\\sRD", " Road", df$Locale)
  df$Locale <- gsub("\\sAVE", " Avenue", df$Locale)
  df$Locale <- gsub("\\sBLVD", " Boulevard", df$Locale)
  df$Locale <- gsub("\\sST", " Street", df$Locale)
  df$Locale <- gsub("\\sHWY", " Highway", df$Locale)
  df$Microhabitat <- gsub("foraging on ground", "on ground", df$Microhabitat)
  return(df)
}



#' This is a wrapper of the writeOGR function that takes a critter club
#' database format and turns it to a spatial points data frame
#' and then converts it to a kml for viewing in Google Earth
#' There is a bug in the KML driver for writeOGR that unfortunately throws a cryptic warning
#' but the code seems to still work
#' @param df a data frame in standard Cal Academy Collection Code Format
#' @param output.path the full path of where the file should be saved
#' @export
write.critter.kml <- function(df, output.path){
  # convert a dataframe to a KML file for viewing in Google Earth
  df %<>% select(Lat, Lon, CollectionCode)
  # turn master data frame into a Spatial Point object and project to WGS84
  df$Lat <- as.numeric(df$Lat)
  df$Lon <- as.numeric(df$Lon)
  coords <- as.data.frame(cbind(Lon = df$Lon, Lat = df$Lat))
  points <- coords
  sp::coordinates(points) <- ~Lon + Lat
  spdf <- sp::SpatialPointsDataFrame(points, df)
  proj4string(spdf) <- sp::CRS("+proj=longlat +ellps=WGS84")
  spdf <- sp::spTransform(spdf, CRS("+proj=longlat +ellps=WGS84"))
  rgdal::writeOGR(obj = spdf, dsn = output.path, layer = "insect", driver = "KML", dataset_options = "NameField=CollectionCode", overwrite_layer = T)
}








#' this function streamlines adding clarifying text to the end of a filename
#' @param full.path a full path to a file
#' @param text the text string you want to add to the file base name
#' @export
add_to_filename <- function(full.path, text){
  basename <- full.path %>% basename()
  dir.name <- full.path %>% dirname()
  file.sans.ext <- basename %>% tools::file_path_sans_ext()
  ext <- basename %>% tools::file_ext()
  new.save.path <- paste0(dir.name, "/", file.sans.ext, text, ".", ext)
  return(new.save.path)
}







#' ifelse function that doesn't obliterate classes (i.e. date formats, etc...)
#' @export
safe_ifelse <- function(cond, yes, no){
  class.y <- class(yes)
  X <- ifelse(cond,yes,no)
  class(X) <-class.y; return(X)
}

#' modified gsub function that allows substitution of lists of key value pairs
#' @export
gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

#' paste function that doesn't paste NA's as strings
#' @export
paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}



#' Last specimen code finder
#'function which takes a collection code and db_sc and returns
#'the last specimen code (highest number) associated with that collection.
#'This function is useful for making sure that specimen codes are not duplicated
#'when generating new ones.
#' @param cc.code a numeric value corresponding to a collection code
#' @param db_sc a data frame file in standard Cal Academy specimen code database format
#' @return a numeric value corresponding to the last assigned specimen code for a given collection code
#' @export
find_max_sc <- function(cc.code, db_sc){
  db_sc$SpecimenCodeBase <- str_match(db_sc$SpecimenCode, '(\\w+#(\\d+))')[,2]
  db_sc$SpecimenCodeNum <- str_match(db_sc$SpecimenCode, '(\\w+#(\\d+))')[,3] %>% as.numeric()
  db_sc$SpecimenCodeSuffix <- str_match(db_sc$SpecimenCode, '(\\w+#\\d+-(\\d+))')[,3]
  sc_query <- db_sc %>% filter(db_sc$SpecimenCodeNum %in% cc.code)
  if(nrow(sc_query) > 0){
    max.sc <- max(sc_query$SpecimenCodeSuffix[]) %>% as.numeric()
    # if a match is found in the specimen code database but there are no split codes yet for that collection code return "1"
    if(is.na(max.sc)){
      max.sc <- 1
    }
    return(max.sc)
  } else cat(paste0("No match in the specimen code database for that collection code!\n"))
}





##### MEDIAWIKI INTERFACING #####

#'function that rearranges a dataframe to a lookup table style format where every
#'unique value across focal columns is represented in the leftmost column of the dataframe
#' @return a vector with results
#' @param df a data frame or tibble
#' @param col_vec a list of strings corresponding to the focal columns to be rearranged
#' @export
lua_dict_builder <- function(df, col_vec){
  df %<>% select(all_of(col_vec)) # select only focal columns

  # for loop to pull distinct values and associated information from each focal column
  df_list <- list()

  for(i in 1:length(col_vec)){
    current_rank <- col_vec[i]
    df_i <- df %>% distinct(across(current_rank), .keep_all = TRUE) %>%
      mutate(index = get(current_rank),
             rank = current_rank) %>%
      filter(!is.na(index))
    first_cols <- df_i %>% select(index, rank)
    other_cols <- df_i %>% select(all_of(col_vec))
    other_cols <- other_cols[, i:length(col_vec)] # this removes entries that are redundant based on rank
    other_cols %<>% select(-all_of(current_rank))
    df_i <- bind_cols(first_cols, other_cols)
    df_i %<>% filter(!is.na(index))
    df_i %<>% filter(index != "")
    df_list[[i]] <- df_i
  }

  df_list %<>% bind_rows()

  return(df_list)
}



#'function that adds brackets appropriately around lists that are nested in each element in a column
#' @param column a data frame column
#' @export
bracketr <- function(column){
  library(glue)
  column <- str_split(column, ",")
  column <- lapply(column, glue_collapse, sep = "]], [[")
  column <- gsub("^", "[[", column)
  column <- gsub("$", "]]", column)
  column %<>% str_remove_all("\\[\\[\\]\\]")
  return(column)
}



#'function that formats a data frame for importation using the Data Transfer extension
#'so that content can be uploaded to MediaWiki
#' @param df a frame to be formatted
#' @param title the "Title" column for Cargo
#' @param free_text the "Free Text" column for Cargo
#' @param prefix a string with the prefix for each field which corresponds to the table name in Cargo
#' @export
cargo_formatr <- function(df, title = "title", free_text = "free_text", prefix){
  prefix %<>% snakecase::to_sentence_case()
  # rename columns to reflect Cargo demands
  df %<>% rename(Title = any_of(title), `Free Text` = any_of(free_text))
  # separate out title and free text columns
  df_no_format <- df %>% select(Title, `Free Text`)
  df %<>% select(-"Title", -"Free Text")
  # replace underscores with spaces in title field
  df_no_format$Title %<>% str_replace("_", " ")
  # format field names to Cargo standard
  names(df) %<>% snakecase::to_sentence_case(sep_out = "_")
  names(df) %<>%
    str_replace("^", paste0(prefix, "[")) %>%
    str_replace("$", "]")
  # recombine data
  df %<>% bind_cols(df_no_format) %>% select(Title, everything(), `Free Text`)
  return(df)
}



#' Function which converts a data frame/tibble to WikiMedia Lua Module format
#' @return a vector with results
#' @param df a data frame or tibble
#' @param id_column a string containing the name of the unique (leftmost) id column
#' @export
lua_df_converter <- function(df, id_column){
  list_output <- list() # initialize list for main output


  # loop which iterates across rows with a nested list that iterates across each col of current row
  for(i in 1:nrow(df)){
    df <- mutate(df, across(everything(), as.character))

    current_row <- df[i,]
    non_id_columns <- current_row %>% select(-id_column) # pull out non-id cols

    # format non-id cols in col name / value pairs
    list_col <- list()

    for(j in 1:ncol(non_id_columns)){
      current_col <- select(non_id_columns, all_of(j))
      list_col[j] <- paste0(names(current_col), " = \"", current_col[[1]], "\"")
    }
    list_col <- paste(list_col, collapse = ", ") # separate non-id column list by commas

    list_output[i] <- paste0("[\"", current_row[[id_column]], "\"] = { ", list_col ," },")
  }

  list_output <- gsub('"\\{', '\\{', list_output)
  list_output <- gsub('\\}"', '\\}', list_output)


  # final formatting of prefix and suffix to body of loop
  prefix <- paste0("return{\n")
  suffix <-  paste0("\n}")
  list_output <- c(prefix, list_output, suffix)

  return(list_output)

}




##### CREATING NICE PLOTS, REPORTS AND TABLE OUTPUT #####

#' This function plots family observation level results by type of animal. The
#' aes_string function was an elegant way to have user input become col_names
#' within a ggplot wrapper
#' @param checklist
#' @param type
#' @param taxonRank
#' @param counts
#' @param dir
#' @return a vector with results
#' @export
graph_at_taxon <- function(checklist, type, taxonRank, counts, dir){
  # this section is used to find out what type of animal and checklist geography the user entered
  animalType <- substitute(type)
  animalTaxonRank <- substitute(taxonRank)
  checklist.name <- substitute(checklist)
  checklist.name <- gsub("clFilled", "", checklist.name)

  if(checklist.name == 'NA'){manual.colors <- c("grey55", "dodgerblue1", "darkgoldenrod1")}
  if(checklist.name == 'AZ'){manual.colors <- c("grey55", "darkgoldenrod1", "dodgerblue1")}

  # filter records
  checklist %<>% filter(type == animalType)

  # settings for ggplots
  fig.width <- 8
  fig.height <- 8
  units <- 'in'
  if(animal.type == 'bird') {
    fig.width <- fig.width + 4
    fig.height <- fig.height + 8
  }

  plot <- ggplot(filter(checklist, type == type), aes_string(taxonRank, fill = counts)) +
    geom_bar(position = "stack") +
    scale_fill_manual(values = manual.colors) +
    theme(legend.position = 'none', axis.text.x = element_text(angle = 0, hjust = 0)) +
    coord_flip()
  print(plot)
  file.name <- paste0(type, '-', taxonRank, '-', checklist.name, '.pdf')
  ggsave(plot = plot, filename = file.path(dir, file.name), device = 'pdf', width = fig.width, height = fig.height, units = units)
  return(plot)
}


#' This function takes a filled checklist, a type of animal, and a taxon rank
#'  and makes a standard critter club checklist report
#' @param filled.cl
#' @param type
#' @param taxon.rank
#' @return a vector with results
#' @export
taxon.summ.by.type <- function(filled.cl, type, taxon.rank){
  summ.df <- filled.cl %>%
    filter(type == type) %>%
    group_by(taxon.rank) %>%
    summarize(Observed = sum(!is.na(eventDate)), Total = n(), Percent = round(Observed / Total * 100, 0))
}



##### PROCESSING AND WRITING LABELS #####

#' Add proper abbreviations, spacing, and punction for insect labels
#' This function takes a label inbox object and writes
#' appropriate label format in text file to serve as input
#' for latex'
#' @param label_inbox standard Cal Academy Insect Dataframe for writing insect labels
#' @return an edited dataframe in preparation for inputing to LaTeX
#' @export
clean_ent_label <- function(label_inbox){

  cat(paste0("Starting label formatting...\n"))

  # convert dates to insect label dates
  label_inbox$DatesCollected <- ento_dates(label_inbox$DateCollectedStart, label_inbox$DateCollectedEnd)

  # COLLECTION LABEL CLEANUP SECTION

  # 2ND ADMIN DISTRICT FORMATTING BY COUNTRY
  label_inbox %<>% mutate(Adm2 = case_when(
    label_inbox$Country == "Mexico" ~ paste0("Mpio. ", label_inbox$Adm2),
    label_inbox$Country == "United States of America" ~ paste0(label_inbox$Adm2, " Co."),
    label_inbox$Country == "Canada" ~ paste0(label_inbox$Adm2," Reg. Dist., ")
  )
  )

  label_inbox$Country <- ifelse(!is.na(label_inbox$Country), paste0(label_inbox$Country,": "), "")
  label_inbox$Adm1 <- ifelse(!is.na(label_inbox$Adm1), paste0(label_inbox$Adm1,", "), "")
  label_inbox$Adm2 <- ifelse(!is.na(label_inbox$Adm2), paste0(label_inbox$Adm2,", "), "")


  # Combine Region and Locale
  label_inbox$Place <- ifelse(is.na(label_inbox$Region), paste0(label_inbox$Locale, ". "), paste0(label_inbox$Region,", ",label_inbox$Locale, ". "))
  # Go back to just using Locale for an entry if Locale + Region passes a specified length
  # This is to dial in to avoid spilling onto a 6th line on the latex labels
  place.char.limit <- 35
  label_inbox$Place <- ifelse(nchar(label_inbox$Place) > place.char.limit,
                              paste0(label_inbox$Locale, ". "),
                              label_inbox$Place
  )


  # for coordinates
  label_inbox <- round_coords(label_inbox) # round coordinate precision to match accuracy
  label_inbox$Lat <- ifelse(!is.na(label_inbox$Lat), paste0(label_inbox$Lat,"° "), "")
  label_inbox$Lon <- ifelse(!is.na(label_inbox$Lon), paste0(label_inbox$Lon,"° "), "")
  label_inbox$Lat <- gsub("-", "\\\\char\"2212 ", label_inbox$Lat) # convert hyphen to true minus sign for latex
  label_inbox$Lon <- gsub("-", "\\\\char\"2212 ", label_inbox$Lon) # convert hyphen to true minus sign for latex

  # for elevation
  label_inbox$Elevation <- ifelse(!is.na(label_inbox$Elevation), paste0(label_inbox$Elevation,"m "), "")

  # dates
  label_inbox$DatesCollected <- ifelse(!is.na(label_inbox$DatesCollected), paste0(label_inbox$DatesCollected," "), "")

  # Customize habitat, microhabitats and methods and how they go on the label

  label_inbox %<>% mutate(Habitat = case_when(
    is.na(label_inbox$Method) & is.na(label_inbox$Microhabitat) ~ paste0(label_inbox$Habitat),
    TRUE ~ paste0(label_inbox$Habitat,", ")
  )
  )

  label_inbox$Method[label_inbox$Method == "hand"] <- NA # remove hand collecting method so it doesn't go on labels
  label_inbox$Method[label_inbox$Method == "net"] <- NA # remove net collecting method so it doesn't go on labels

  label_inbox %<>% mutate(micro_method = case_when(
    !is.na(label_inbox$Method) & !is.na(label_inbox$Microhabitat) ~ paste0(label_inbox$Microhabitat,", ",label_inbox$Method, "."),
    !is.na(label_inbox$Method) & is.na(label_inbox$Microhabitat) ~ paste0(label_inbox$Method, "."),
    is.na(label_inbox$Method) & !is.na(label_inbox$Microhabitat) ~ paste0(label_inbox$Microhabitat, "."),
    is.na(label_inbox$Method) & is.na(label_inbox$Microhabitat) ~ paste0("")
  )
  )


  # STANDARD ABBREVIATIONS FOR LABELS (THINGS THAT I DON'T WANT CHANGED ON THE ORIGINAL RECORDS BUT DO WANT CHANGED FOR LABELS

  label_inbox$Country <- gsub("Mexico", "MEX", label_inbox$Country) # based on ISO standardized nation 3-letter abbreviations
  label_inbox$Country <- gsub("Canada", "CAN", label_inbox$Country) # based on ISO standardized nation 3-letter abbreviations
  label_inbox$Country <- gsub("Argentina", "ARG", label_inbox$Country) # based on ISO standardized nation 3-letter abbreviations
  label_inbox$Country <- gsub("United States of America", "USA", label_inbox$Country) # based on ISO standardized nation 3-letter abbreviations
  label_inbox$Country <- gsub("Falkland Islands", "FLK", label_inbox$Country) # based on ISO standardized nation 3-letter abbreviations

  label_inbox$Adm1 <- gsub2(state.name, state.abb, label_inbox$Adm1)
  label_inbox$Adm1 <- gsub("Baja California Norte", "BCN", label_inbox$Adm1)
  label_inbox$Adm1 <- gsub("Baja California Sur", "BCS", label_inbox$Adm1)
  label_inbox$Adm1 <- gsub("Catamarca", "CT", label_inbox$Adm1)
  label_inbox$Adm1 <- gsub("La Rioja", "LR", label_inbox$Adm1)
  label_inbox$Adm1 <- gsub("Sonora", "SON", label_inbox$Adm1)
  label_inbox$Adm1 <- gsub("British Columbia", "BC", label_inbox$Adm1)

  label_inbox$Place <- gsub("kilometers", "km", label_inbox$Place)
  label_inbox$Place <- gsub("National Forest", "Natl. For.", label_inbox$Place)
  label_inbox$Place <- gsub("National Monument", "Natl. Mon.", label_inbox$Place)
  label_inbox$Place <- gsub("National Park", "Natl. Pk.", label_inbox$Place)
  label_inbox$Place <- gsub("Recreation Area", "Rec. Area", label_inbox$Place)
  label_inbox$Place <- gsub("Recreation ", "Rec. ", label_inbox$Place)
  label_inbox$Place <- gsub("National Conservation Area", "Natl. Cons. Area.", label_inbox$Place)
  label_inbox$Place <- gsub("Wilderness Area", "Wldrns.", label_inbox$Place)
  label_inbox$Place <- gsub("Fort//s", "Ft.", label_inbox$Place)
  label_inbox$Place <- gsub("Southwestern Research Station", "SW Research Stn.", label_inbox$Place)
  label_inbox$Place <- gsub("University", "Univ.", label_inbox$Place)
  label_inbox$Place <- gsub("South Fork", "S. Fork", label_inbox$Place)

  label_inbox$Place <- gsub("National", "Natl.", label_inbox$Place)
  label_inbox$Place <- gsub("Center", "Cen.", label_inbox$Place)

  label_inbox$Place <- gsub("\\snorth\\s", " N ", label_inbox$Place)
  label_inbox$Place <- gsub("\\ssouth\\s", " S ", label_inbox$Place)
  label_inbox$Place <- gsub("\\seast\\s", " E ", label_inbox$Place)
  label_inbox$Place <- gsub("\\swest\\s", " W ", label_inbox$Place)

  label_inbox$Place <- gsub("\\sMountains", " Mts.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sMountain", " Mt.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sCanyon", " Cyn.", label_inbox$Place, ignore.case = TRUE)

  label_inbox$Place <- gsub("\\sRoad", " Rd.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sStreet", " St.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sRoute", " Rte.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sAvenue", " Ave.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sBoulevard", " Blvd.", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sHighway", " Hwy.", label_inbox$Place)
  label_inbox$Place <- gsub("\\sTrail[ \\.\\,]", " Trl. ", label_inbox$Place)

  label_inbox$Place <- gsub("\\sdegrees\\s", "° ", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sand\\s", " & ", label_inbox$Place)

  # other misc. substitutions
  label_inbox$Method <- gsub("hand\\\aspirator", " hand", label_inbox$Method, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sSprings Campground", " Spgs. Campground", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("\\sSpring Campground", " Spg. Campground", label_inbox$Place, ignore.case = TRUE)
  label_inbox$Place <- gsub("Campground", "Campgrnd.", label_inbox$Place, ignore.case = TRUE)

  # get rid of double periods caused by abbreviations at the end of a sentence
  label_inbox$Place <- gsub("\\.\\.", ".", label_inbox$Place)
  label_inbox$Place <- gsub("\\s\\,", ",", label_inbox$Place)
  label_inbox$Place <- gsub("\\s\\.", ".", label_inbox$Place)
  label_inbox$micro_method <- gsub("\\.\\.", ".", label_inbox$micro_method)

  # format Collection Code for label
  label_inbox$Code <- gsub("RPO#", "", label_inbox$Code) # remove prefix to Collection Code
  label_inbox$Code <- gsub("-\\d+", "", label_inbox$Code) # temporarily remove hyphen and subcode to avoid formatting problems
  # add leading zeros with formatC
  label_inbox$Code <- ifelse(!is.na(label_inbox$Code),
                             paste0("Overson RPO#", formatC(label_inbox$Code, width=4, format="d", flag="")),
                             label_inbox$Code)
  # add sub codes back after formating leading zeros with formatC
  label_inbox$Code <- ifelse(!is.na(label_inbox$SubCode),
                             paste0(label_inbox$Code, "-", label_inbox$SubCode),
                             label_inbox$Code
  )
  return(label_inbox)
}



#' Write insect labels
#' This function takes a label inbox tsv file and writes
#' appropriate label format in text file to serve as input
#' for latex to build insect labels
#' @param label_inbox standard Cal Academy Insect Dataframe for writing insect labels
#' @return A text output readable by latex
#' @export
write_latex_labels <- function(label_inbox){
  # check that collection code input has at least one row
  if(nrow(label_inbox) < 1){
    cat(paste0("no collection codes found in label input\n"))
    stop()
  }

  # make a test dataframe of most crucial columns to check for missing values before function runs
  check_empty(label_inbox)

  # abbreviate text for label formatting and other changes
  label_inbox <- clean_ent_label(label_inbox)

  # writing data to latex data files #
  # add remark instructions to characters with special meaning for latex before writing to latex input datafile
  label_inbox <- as.data.frame(lapply(label_inbox,function(x) if(is.character(x)|is.factor(x)) gsub("\\&","\\\\&",x) else x))
  label_inbox <- as.data.frame(lapply(label_inbox,function(x) if(is.character(x)|is.factor(x)) gsub("\\%","\\\\%",x) else x))
  label_inbox <- as.data.frame(lapply(label_inbox,function(x) if(is.character(x)|is.factor(x)) gsub("\\$","\\\\$",x) else x))
  label_inbox <- as.data.frame(lapply(label_inbox,function(x) if(is.character(x)|is.factor(x)) gsub("\\#","\\\\#",x) else x))
  label_inbox <- as.data.frame(lapply(label_inbox,function(x) if(is.character(x)|is.factor(x)) gsub("\\_","\\\\_",x) else x))

  label_inbox <- label_inbox[with(label_inbox, order(Code)),] # sort cc_new

  # start label processing

  cat(paste0("starting label writing process to latex_label_input.txt\n\n"))
  # export collection label info to a text document
  counter <<- 0
  file.path <- paste0('./labels/latex/latex_label_input.txt')
  if (file.exists(file.path)){
    file.remove(file.path)
  }
  for(i in 1:nrow(label_inbox)){
    sink(file.path, append = TRUE)
    cat(paste0("\\insectlabel{\\textbf{", label_inbox$Country[i], label_inbox$Adm1[i], "}", label_inbox$Adm2[i],
               label_inbox$Place[i], label_inbox$Lat[i], label_inbox$Lon[i], label_inbox$Elevation[i],
               label_inbox$DatesCollected[i], label_inbox$Code[i], "}\\"))
    cat(paste0("\n"))
    cat(paste0("\n"))
    cat(paste0("\\insectlabel{", label_inbox$Habitat[i], label_inbox$micro_method[i], "}"))
    cat(paste0("\n"))
    cat(paste0("\n"))
    sink()
    counter <<- counter + 1
  }

  # run xelatex command to produce labels
  # working directory has to be set as such or xelatex will add auxilary files to the wrong path
  setwd('./labels/latex/')
  system('xelatex insect_label.tex')
  setwd('../../')
}









