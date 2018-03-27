## Move the survey files from the SD card to a hard drive.
list.files(path = "/Volumes/Untitled 1/Stand 33 Audio_loc13/Data")
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Stand 33 Audio_loc13/Data",
               to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
               csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "W33L13",
               ext = "wav", CardRecorderID = 6, rec.tz = "US/Eastern")

## As with other upload functions, I needed to rewrite the function. It had two problems:
## 1) It again tried to insert NULL into a primary key column that cannot be null (pkSurveyID)
## 2) it pasted a weird "//t" character into the INSERT statement.
dbUploadSurvey <- function (db.name, uid, pwd, survey.meta, update.query = FALSE, 
                            tz, ...) 
{
  if (!requireNamespace("RODBC", quietly = TRUE)) {
    stop("The RODBC package is needed to use this function, but it is not installed. Please install it.", 
         call. = FALSE)
  }
  start.time <- Sys.time()
  if (missing(uid) && missing(pwd)) {
    dbCon <- RODBC::odbcConnect(db.name, ...)
  }
  else if (missing(uid)) {
    dbCon <- RODBC::odbcConnect(db.name, pwd, ...)
  }
  else dbCon <- RODBC::odbcConnect(db.name, uid, pwd, ...)
  on.exit(close(dbCon))
  if (!"fldOriginalDateModified" %in% names(survey.meta)) {
    date.time.info <- regmatches(survey.meta[, "fldSurveyName"], 
                                 regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}[ _][0-9]{6}[ _][A-Z][SDM]T", 
                                         survey.meta[, "fldSurveyName"]))
    if (length(date.time.info) >= 1 && nchar(date.time.info) == 
        21) {
      dm <- substr(survey.meta[, "fldSurveyName"], start = 8, 
                   stop = 24)
      dm <- as.Date(dm, format = "%Y-%m-%d_%H%M%S")
      dm <- as.POSIXct(dm, tz = tz, format = "%Y-%m-%d_%H%M%S %Z")
      survey.meta["fldOriginalDateModified"] <- as.character(dm, 
                                                             format = "%Y-%m-%d_%H%M%S %Z")
    }
    else stop("No time in metadata and file name does not have date and time info.")
  }
  if (!update.query) {
    mtimes <- survey.meta[, "fldOriginalDateModified"]
    dates <- substr(mtimes, start = 1, stop = 11)
    hh <- substr(mtimes, start = 12, stop = 13)
    mm <- substr(mtimes, start = 14, stop = 15)
    ss <- substr(mtimes, start = 16, stop = 17)
    date.time <- paste0(unlist(dates), unlist(hh), ":", unlist(mm), 
                        ":", unlist(ss))
    tzone <- substr(mtimes, start = 19, stop = 21)
  }
  empty <- !c("fkCardRecorderID", "fldSurveyLength", "fldOriginalDateModified", 
              "fldTimeZone", "fldOriginalRecordingName", "fldSurveyName", 
              "fldRecordingFormat", "fldSampleRate", "fldBitsperSample", 
              "fldChannels") %in% names(survey.meta)
  empty <- c("fkCardRecorderID", "fldSurveyLength", "fldOriginalDateModified", 
             "fldTimeZone", "fldOriginalRecordingName", "fldSurveyName", 
             "fldRecordingFormat", "fldSampleRate", "fldBitsperSample", 
             "fldChannels")[empty]
  survey.meta[empty] <- NA
  if (!update.query) {
    query <- paste("INSERT INTO `tblSurvey` (`fkCardRecorderID`, `fldSurveyLength`, `fldOriginalDateModified`, `fldTimeZone`, `fldOriginalRecordingName`, `fldSurveyName`, `fldRecordingFormat`, `fldSampleRate`, `fldBitsperSample`, `fldChannels`) VALUES ('", 
                   paste(survey.meta$fkCardRecorder, "', '", 
                         survey.meta$fldSurveyLength, "', '", date.time, 
                         "', '", tzone, "', '", survey.meta$fldOriginalRecordingName, 
                         "', '", survey.meta$fldSurveyName, "', '", survey.meta$fldRecordingFormat, 
                         "', '", survey.meta$fldSampleRate, "', '", 
                         survey.meta$fldBitsperSample, "', '", survey.meta$fldChannels, 
                         "')", sep = "", collapse = ", ('"), sep = "")
  }
  else {
    query <- paste("UPDATE `tblSurvey` SET `fldSurveyLength` = '", 
                   survey.meta$fldSurveyLength, "', `fldSampleRate` = '", 
                   survey.meta$fldSampleRate, "', `fldBitsperSample` = '", 
                   survey.meta$fldBitsperSample, "', `fldChannels` = '", 
                   survey.meta$fldChannels, "' WHERE `fldSurveyName` = '", 
                   survey.meta$fldSurveyName, "'", sep = "")
  }
  if (update.query) {
    status <- lapply(X = query, FUN = RODBC::sqlQuery, channel = dbCon)
  }
  else {
    status <- RODBC::sqlQuery(dbCon, query)
  }
  message(if (is.na(status[1])) {
    paste("Done! Upload time:", round(Sys.time() - start.time, 
                                      2), "seconds")
  }
  else if (status[1] == "character(0)") {
    paste("Done! Upload time:", round(Sys.time() - start.time, 
                                      2), "seconds")
  }
  else paste("Upload unsuccessful; RODBC returned errors: ", 
             paste(status, collapse = " ")))
}



dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#W34L10
list.files(path = "/Volumes/Untitled 1/")
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Stand 34 Audio_loc10/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "W34L10",
                           ext = "wav", CardRecorderID = 7, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#W43L12
list.files(path = "/Volumes/Untitled 1/")
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "W43L12",
                           ext = "wav", CardRecorderID = 10, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS43L6
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS43L6",
                           ext = "wav", CardRecorderID = 8, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS46L1
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS46L1",
                           ext = "wav", CardRecorderID = 5, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS35L9
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Stand 35 Audio_loc9/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS35L9",
                           ext = "wav", CardRecorderID = 3, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS28L8
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Stand 28 Audio_loc8/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS28L8",
                           ext = "wav", CardRecorderID = 9, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#W28L11
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "W28L11",
                           ext = "wav", CardRecorderID = 12, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS41L5
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS41L5",
                           ext = "wav", CardRecorderID = 4, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS44L2
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS44L2",
                           ext = "wav", CardRecorderID = 11, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS43L4
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS43L4",
                           ext = "wav", CardRecorderID = 2, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")

#WS33L7
metadata <- fileCopyRename(from = "/Volumes/Untitled 1/Data",
                           to = "/Volumes/Untitled", csv.dir = "/Users/johnlloyd/GitHub/BITH_Maine",
                           csv.name = "surveyMetadataMaine2016.csv", loc.prefix = "WS33L7",
                           ext = "wav", CardRecorderID = 1, rec.tz = "US/Eastern")
dbUploadSurvey(survey.meta = metadata, db.name = "noh", uid = "root",
               pwd = "RSW4xN!!")
