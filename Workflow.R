rm(list=ls())
library(monitoR)
#Make and test templates.
##Template creation.
bithTemplate1 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall3.wav",
                                 name = "c1", frq.lim = c(3,4.75))
bithTemplate2 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong7.wav",
                                  name = "s1", frq.lim = c(3,9))
bithTemplate3 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong8.wav",
                                  name = "s2", frq.lim = c(4.5,8))

bithTemp <- combineCorTemplates(bithTemplate1, bithTemplate2, bithTemplate3)

dbUploadTemplate(templates = bithTemp,
                 uid = "root",
                 pwd = "RSW4xN!!",
                 db.name = "noh",
                 analyst = 1, 
                 locationID = "2",
                 date.recorded = "2017/09/07",
                 recording.equip = "Unknown",
                 species.code = "BITH",
                 type = "COR")

##Template testing.
###Survey is from Mansfield, 2016, and contains singing and calling Bicknell's. 
scores.test <-corMatch(survey = "S4A01574_20160604_210803.wav", templates = bithTemp,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
detects.test <- findPeaks(score.obj = scores.test, parallel = T)
detects.test
plot(detects.test, t.each = 120)
df <- data.frame(getDetections(detects.test))



##
surveylist <- list.files(path = "/Volumes/Untitled", pattern = "WS33L7")
scores <- vector("list", length(surveylist))
for(i in 1:length(surveylist)){
  scores[[i]] <- corMatch(survey = paste0("/Volumes/Untitled/",surveylist[i]), templates = bithTemp, parallel = T, show.prog = TRUE,
                        cor.method = "pearson")
}

detects <- vector("list", length(surveylist))
for(i in 1:length(surveylist)){
  detects[[i]] <- findPeaks(score.obj = scores[[i]], parallel = T)
}

##I couldn't get the dbUploadResult function to work, so here 
##I've pulled out the needed arguments. Initially, this has to be done
##manually for each detection list in the grand detection list. 

## Uploading results for first survey results:
detects.sub <- detects[[1]]
detects.sub@survey.name <- "WS33L7_2017-06-05_215700_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                  output = "list", which.one = "c1")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]
##rbindf
rbindf <- function(...) {
  
  l <- list(...)
  if(length(l) == 1) l <- l[[1]]
  nn <- length(l)
  
  x <- l[[1]]
  if(length(l)>1){
    for(i in 2:nn) {
      y <- l[[i]]
      if(!all(yinx <- names(y) %in% names(x))) {
        x[, names(y)[!yinx]] <- NA
      } 
      if(!all(xiny <- names(x) %in% names(y))) {
        y[, names(x)[!xiny]] <- NA
      } 
      x <- rbind(x, y)
    }
  }
  return(x)
}
##
pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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

## Second result:
detects.sub <- detects[[2]]
detects.sub@survey.name <- "WS33L7_2017-06-06_062700_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                       output = "list", which.one = "c1")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]

pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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


## Third result, for c1:
detects.sub <- detects[[3]]
detects.sub@survey.name <- "WS33L7_2017-06-06_215700_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                       output = "list", which.one = "c1")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]

pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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

## Third result, for s2:
detects.sub <- detects[[3]]
detects.sub@survey.name <- "WS33L7_2017-06-06_215700_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                       output = "list", which.one = "s2")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]

pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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
close(dbCon)

## Fourth result:
detects.sub <- detects[[4]]
detects.sub@survey.name <- "WS33L7_2017-06-07_062600_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                       output = "list", which.one = "s2")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]

pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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
close(dbCon)

## Fifth result:
detects.sub <- detects[[5]]
detects.sub@survey.name <- "WS33L7_2017-06-07_215800_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                       output = "list", which.one = "s2")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]

pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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
close(dbCon)

## Sixth result:
detects.sub <- detects[[6]]
detects.sub@survey.name <- "WS33L7_2017-06-08_062600_EDT_00000_000.wav"
pks.L <- getDetections(detection.obj = detects.sub, 
                       output = "list", which.one = "s2")
analysis.type <- "COR"
dbCon <- RODBC::odbcConnect(dsn = "noh", uid = "root", pwd = "RSW4xN!!")
#which.one <- names(pks.L)
#which.one <- "c1"
cutoff <- lapply(detects.sub@templates, function(x) x@score.cutoff)
templateID <- unlist(lapply(pks.L, function(x) unique(x$template)))
template.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkTemplateID`, `fldTemplateName` FROM `tblTemplate` WHERE `fldTemplateName` = '", 
                                             paste(names(pks.L), sep = "", collapse = "' OR `fldTemplateName` = '"), 
                                             "'", sep = ""))
templateID[1] <- template.dat$pkTemplateID[template.dat$fldTemplateName == 
                                             templateID[1]]
survey.name <- detects.sub@survey.name
survey.dat <- RODBC::sqlQuery(dbCon, paste("Select `pkSurveyID`, `fldSurveyName` FROM `tblSurvey` WHERE `fldSurveyName` = '", 
                                           paste(survey.name, sep = "", collapse = "' OR `fldSurveyName` = '"), 
                                           "'", sep = ""))
survey.name[1] <- survey.dat$pkSurveyID[survey.dat$fldSurveyName == 
                                          survey.name[1]]
pks.L[[1]]$template <- templateID[[1]]
pks.L[[1]]$score.cutoff <- cutoff[[1]]

pks.L <- rbindf(pks.L)
date.time <- unlist(lapply(X = pks.L$date.time, FUN = substr, 
                           start = 1, stop = 19))
tzone <- unlist(lapply(pks.L$date.time, function(x) as.character(x, 
                                                                 format = "%Z")))
start.time <- Sys.time()
query <- paste("INSERT INTO `tblResult` (`fkSurveyID`, `fkTemplateID`, `fkPersonID`, `fldDateTime`, `fldTimeZone`, `fldTime`, `fldScore`, `fldAnalysisType`,`fldCutoffValue`) VALUES ('", 
               paste(survey.name, "', '", pks.L$template, 
                     "', '", analyst, "', '", date.time, "', '", tzone, 
                     "', '", pks.L$time, "', '", pks.L$score, "', '",
                     analysis.type, "', '", 
                     pks.L$score.cutoff, "')", sep = "", collapse = ", ('"), 
               sep = "")
status <- RODBC::sqlQuery(dbCon, query)
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
close(dbCon)

##Clean up
rm (detects, detects.sub, detects.test, scores, scores.test)

