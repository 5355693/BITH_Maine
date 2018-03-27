library(warbleR)
library(ggplot2)

## These templates were downloaded from Xeno-Canto, and were recorded on Mount Mansfield.
bithTemplate1 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHsong2.wav",
                                 name = "s1") #, frq.lim = c(2.5,9)) ##frq.lim settings can greatly affect results! 
bithTemplate2 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHCall1.wav",
                                 name = "c1") #, frq.lim = c(3,5.5))

bithTemps <- combineCorTemplates(bithTemplate1,bithTemplate2)

cscores <-corMatch(survey = "S4A01574_20160604_210803.wav", templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)

## The templates don't work well: the call records peaks everywhere, and the song misses everything.
## Adjust the frequency limits.

bithTemplate1 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong2.wav",
                                 name = "s2", frq.lim = c(2,10)) ##frq.lim settings can greatly affect results! 
bithTemplate2 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall1.wav",
                                 name = "c1", frq.lim = c(3.5,5.5))
## Add a few more clipped from a Mansfield ARD, with noise reduction applied in Audacity
bithTemplate3 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong3.wav",
                                 name = "s3", frq.lim = c(4,9)) ##frq.lim settings can greatly affect results! 
bithTemplate4 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong4.wav",
                                 name = "s4", frq.lim = c(2,10))
bithTemplate5 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong5.wav",
                                 name = "s5", frq.lim = c(2,10))
bithTemplate6 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong6.wav",
                                 name = "s6", frq.lim = c(2,10)) ##frq.lim settings can greatly affect results! 
#bithTemplate7 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall2.wav",
#                                 name = "c2", frq.lim = c(3,7))
bithTemplate8 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall3.wav",
                                 name = "c3", frq.lim = c(3,4.75))
bithTemplate9 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall4.wav",
                                 name = "c4", frq.lim = c(2,7))
bithTemplate10 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall5.wav",
                                 name = "c5", frq.lim = c(2,7))
bithTemplate11 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall6.wav",
                                  name = "c6", frq.lim = c(2,8))
bithTemplate12 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong1.wav",
                                  name = "s1", frq.lim = c(2,10))
bithTemplate13 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong7.wav",
                                  name = "s7", frq.lim = c(3,9))
bithTemplate14 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong8.wav",
                                  name = "s8", frq.lim = c(4.5,8))

bithTemps <- combineCorTemplates(bithTemplate8, bithTemplate13, bithTemplate14)


cscores <-corMatch(survey = "S4A01574_20160604_210803.wav", templates = bithTemplate14,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
getDetections(cdetects)
plot(cdetects)

## Call 4 seems to find a lot of non-BITH calls. Also noteworthy how poorly the song
## recognizers work.

##How perform with a Maine recording
## c3 = 23; s7 = 0, s8 = 3
cscores <-corMatch(survey = "/Volumes/Untitled/W33L13_2017-06-07_045300_EDT.wav",
                   templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects, t.each = 120)
getDetections(cdetects)

##How perform with a MANS recording
## c3 = 100; s7 = 7, s8 = 6
cscores <-corMatch(survey = "S4A01574_20160604_210803.wav",
                   templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)
getDetections(cdetects)

## Similar pattern here: Call 4 has numerous false positives, many of which are coming
## on Magnolia Warbler songs and Fox Sparrow songs. Call 3 works well: it picks up 3 
## odd, churry SWTH calls and 2 cracking branches that made noise that cover the spectrum - 
## every recognizer picked up those. On the Mansfield recording with BITH, it detected 100
## calls - on this recoring, w/out BITH, it detected only 5. The other recognizers didn't
## show the same level of sensitivity.

## Try another one.
## Works pretty well.
cscores <-corMatch(survey = "/Volumes/Untitled/W33L13_2017-06-05_212800_EDT.wav",
                   templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)
getDetections(cdetects)

## Again, c4 overdetects, finding many SWTH calls.
## c3 finds 7 SWTH calls, which interestingly are "whit-burr" calls,
## which seem to elicit particular confusion with the template. 

cscores <-corMatch(survey = "/Volumes/Untitled/W33L13_2017-06-06_045400_EDT.wav",
                   templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)
getDetections(cdetects)

## This one is harder, with even the well-performing c3 picking up many false negatives, 
## including the usual SWTH "churr" call but also a BHVI song. 
cscores <-corMatch(survey = "/Volumes/Untitled/W33L13_2017-06-06_055400_EDT.wav",
                   templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)
getDetections(cdetects)
bithTemplate1.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong2.wav",
                                 name = "s2", frq.lim = c(4,8)) ##frq.lim settings can greatly affect results! 
bithTemplate2.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall1.wav",
                                 name = "c1", frq.lim = c(3.5,5.5))
## Add a few more clipped from a Mansfield ARD, with noise reduction applied in Audacity
bithTemplate3.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong3.wav",
                                 name = "s3", frq.lim = c(4.5,7.5)) ##frq.lim settings can greatly affect results! 
bithTemplate4.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong4.wav",
                               name = "s4", frq.lim = c(3,9))
bithTemplate5.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong5.wav",
                                 name = "s5", frq.lim = c(3,8))
bithTemplate6.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong6.wav",
                                 name = "s6", frq.lim = c(3,8)) ##frq.lim settings can greatly affect results! 
bithTemplate7.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall2.wav",
                               name = "c2", frq.lim = c(4,5.5))
bithTemplate8.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall3.wav",
                                 name = "c3", frq.lim = c(3,4.75))
bithTemplate9.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall4.wav",
                                 name = "c4", frq.lim = c(3.5,5.5))
bithTemplate10.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall5.wav",
                                  name = "c5", frq.lim = c(2,6))
bithTemplate11.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithCall6.wav",
                                  name = "c6", frq.lim = c(2,6))
bithTemplate12.b <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/bithSong1.wav",
                                  name = "s1", frq.lim = c(3,8))

bithTemps.b <- combineBinTemplates(bithTemplate1.b,bithTemplate2.b, bithTemplate3.b,
                                 bithTemplate4.b, bithTemplate5.b, bithTemplate6.b, bithTemplate7.b,
                                 bithTemplate8.b, bithTemplate9.b, bithTemplate10.b, bithTemplate11.b,
                                 bithTemplate12.b)

##How does a binary template work? Finds nothing on the Maine site.
cscores <-binMatch(survey = "/Volumes/Untitled/W33L13_2017-06-05_212800_EDT.wav",
                   templates = bithTemps.b,
                   parallel = T, show.prog = TRUE)
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)
getDetections(cdetects)

##How about Mansfield? Doesn't seem to work nearly as well - c3 has only 12 detections~
cscores <-binMatch(survey = "S4A01574_20160604_210803.wav", templates = bithTemps.b,
                   parallel = T, show.prog = TRUE)
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)
getDetections(cdetects)

## To upload this template, I need to create my own version of the function in monitoR. 
## The default dbUploadTemplate function doesn't work, because it tries 
## to INSERT a "NULL" into the pkTemplateID column. That column
## auto-increments, so works only if the field is left empty. The only way
## I could figure out how to do this was to change the SQL INSERT command 
## so that it didn't refer at all to pkTemplateID, which effectivel left it blank
## and able to auto-increment after the INSERT completed.

dbUploadTemplate <- 
  function (templates, which.one, db.name = "acoustics", uid, pwd, 
            analyst, locationID = "", date.recorded = "", recording.equip = "", 
            species.code, type, ...) 
  {
    if (!requireNamespace("RODBC", quietly = TRUE)) {
      stop("The RODBC package is needed to use this function, but it is not installed. Please install it.", 
           call. = FALSE)
    }
    start.time <- Sys.time()
    if (tolower(type) %in% c("bin", "bt", "binary", "b")) {
      type <- "BIN"
    }
    else if (tolower(type) %in% c("cor", "ct", "correlation", 
                                  "c")) {
      type <- "COR"
    }
    else stop("Did not recognize type, was it BIN or COR?")
    if (missing(uid) && missing(pwd)) {
      dbCon <- RODBC::odbcConnect(db.name, ...)
    }
    else if (missing(pwd)) {
      dbCon <- RODBC::odbcConnect(db.name, pwd, ...)
    }
    else dbCon <- RODBC::odbcConnect(db.name, uid, pwd, ...)
    on.exit(close(dbCon))
    if (missing(which.one)) 
      template.L <- templates@templates
    else template.L <- templates@templates[names(templates@templates) == 
                                             which.one]
    if (length(species.code) > 1 & length(species.code) != length(names(template.L))) 
      stop("You entered ", length(species.code), " species codes but are uploading ", 
           length(names(template.L)), " templates, this can't be right.")
    species <- RODBC::sqlQuery(dbCon, paste("SELECT `pkSpeciesID`, `fldSpeciesCode` FROM `tblSpecies` WHERE `fldSpeciesCode` = '", 
                                            paste(species.code, sep = "", collapse = "' OR `fldSpeciesCode` = '"), 
                                            "'", sep = ""))
    speciesID <- NULL
    for (i in 1:length(species.code)) {
      speciesID[i] <- species$pkSpeciesID[species$fldSpeciesCode == 
                                            species.code[i]]
    }
    clips <- lapply(template.L, function(x) x@clip.path)
    srates <- lapply(template.L, function(x) x@samp.rate)
    if (type == "BIN") {
      pts.on <- lapply(template.L, function(x) x@pt.on)
    }
    if (type == "BIN") {
      pts.off <- lapply(template.L, function(x) x@pt.off)
    }
    if (type == "COR") {
      pts <- lapply(template.L, function(x) x@pts)
    }
    t.steps <- lapply(template.L, function(x) x@t.step)
    frq.steps <- lapply(template.L, function(x) x@frq.step)
    n.t.bins <- lapply(template.L, function(x) x@n.t.bins)
    first.t.bin <- lapply(template.L, function(x) x@first.t.bin)
    n.frq.bins <- lapply(template.L, function(x) x@n.frq.bins)
    durations <- lapply(template.L, function(x) x@duration)
    frq.lims <- lapply(template.L, function(x) x@frq.lim)
    wls <- lapply(template.L, function(x) x@wl)
    ovlps <- lapply(template.L, function(x) x@ovlp)
    wns <- lapply(template.L, function(x) x@wn)
    score.cutoffs <- lapply(template.L, function(x) x@score.cutoff)
    comments <- lapply(template.L, function(x) x@comment)
    if (type == "BIN") {
      pt.on.L <- lapply(template.L, function(x) x@pt.on)
      pt.off.L <- lapply(template.L, function(x) x@pt.off)
      pt.on.t <- lapply(pt.on.L, function(x) x[, "t"])
      pt.on.f <- lapply(pt.on.L, function(x) x[, "frq"])
      pt.off.t <- lapply(pt.off.L, function(x) x[, "t"])
      pt.off.f <- lapply(pt.off.L, function(x) x[, "frq"])
    }
    else if (type == "COR") {
      pts.L <- lapply(template.L, function(x) x@pts)
      pts.t <- lapply(pts.L, function(x) x[, "t"])
      pts.f <- lapply(pts.L, function(x) x[, "frq"])
      pts.a <- lapply(pts.L, function(x) x[, "amp"] * -100)
    }
    query <- paste0("INSERT INTO `tblTemplate` (`fkSpeciesID`, `fkPersonID`, `fkLocationID`, `fldTemplateName`, `fldRecordingDate`, `fldRecordingEquipment`, `fldClipPath`, `fldSampRate`, `fldPtOnT`, `fldPtOnFrq`, `fldPtOffT`, `fldPtOffFrq`, `fldPtsT`, `fldPtsFrq`, `fldPtsAmp`, `fldTStep`, `fldFrqStep`, `fldNTBins`, `fldFirstTBin`, `fldNFrqBins`, `fldDuration`, `fldFrqLim`, `fldFFTwl`, `fldFFTovlp`, `fldFFTwn`, `fldScoreCutoff`, `fldTemplateType`, `fldActive`, `fldComment`) VALUES ('", 
                    paste0(speciesID, "', '", analyst, "', '", 
                           locationID, "', '", names(template.L), "', '", date.recorded, 
                           "', '", recording.equip, "', '", clips, "', '", srates, 
                           "', '", if (type == "BIN") {
                             pt.on.t
                           }
                           else {
                             ""
                           }, "', '", if (type == "BIN") {
                             pt.on.f
                           }
                           else {
                             ""
                           }, "', '", if (type == "BIN") {
                             pt.off.t
                           }
                           else {
                             ""
                           }, "', '", if (type == "BIN") {
                             pt.off.f
                           }
                           else {
                             ""
                           }, "', '", if (type == "COR") {
                             pts.t
                           }
                           else {
                             ""
                           }, "', '", if (type == "COR") {
                             pts.f
                           }
                           else {
                             ""
                           }, "', '", if (type == "COR") {
                             pts.a
                           }
                           else {
                             ""
                           }, "', '", t.steps, "', '", frq.steps, "', '", n.t.bins, 
                           "', '", first.t.bin, "', '", n.frq.bins, "', '", 
                           durations, "', '", frq.lims, "', '", wls, "', '", 
                           ovlps, "', '", wns, "', '", score.cutoffs, "', '", 
                           type, "', ", 1, ", '", comments, "')", collapse = ", ('"))
    message("Uploading...")
    status <- RODBC::sqlQuery(dbCon, query)
    message("Cleaning up...")
    query <- paste("UPDATE `tblTemplate` SET `fldPtOnT` = REPLACE( `fldPtOnT` , ' ' , '' ), `fldPtOnFrq` = REPLACE( `fldPtOnFrq` , ' ' , '' ), `fldPtOffT` = REPLACE( `fldPtOffT` , ' ' , '' ), `fldPtOffFrq` = REPLACE( `fldPtOffFrq` , ' ' , '' ), `fldPtsT` = REPLACE( `fldPtsT` , ' ' , '' ), `fldPtsFrq` = REPLACE( `fldPtsFrq` , ' ' , '' ), `fldPtsAmp` = REPLACE( `fldPtsAmp` , ' ' , '' ) WHERE `fldTemplateName` = '", 
                   names(template.L), "'", sep = "")
    lapply(query, function(x) RODBC::sqlQuery(dbCon, x))
    query <- paste("UPDATE `tblTemplate` SET `fldPtOnT` = REPLACE( `fldPtOnT` , '\\\\n' , '' ), `fldPtOnFrq` = REPLACE( `fldPtOnFrq` , '\\\\n' , '' ), `fldPtOffT` = REPLACE( `fldPtOffT` , '\\\\n' , '' ), `fldPtOffFrq` = REPLACE( `fldPtOffFrq` , '\\\\n' , '' ), `fldPtsT` = REPLACE( `fldPtsT` , '\\\\n' , '' ), `fldPtsFrq` = REPLACE( `fldPtsFrq` , '\\\\n' , '' ), `fldPtsAmp` = REPLACE( `fldPtsAmp` , '\\\\n' , '' ) WHERE `fldTemplateName` = '", 
                   names(template.L), "'", sep = "")
    lapply(query, function(x) RODBC::sqlQuery(dbCon, x))
    query <- paste("UPDATE `tblTemplate` SET `fldPtOnT` = REPLACE( `fldPtOnT` , '\\\\r' , '' ), `fldPtOnFrq` = REPLACE( `fldPtOnFrq` , '\\\\r' , '' ), `fldPtOffT` = REPLACE( `fldPtOffT` , '\\\\r' , '' ), `fldPtOffFrq` = REPLACE( `fldPtOffFrq` , '\\\\r' , '' ), `fldPtsT` = REPLACE( `fldPtsT` , '\\\\r' , '' ), `fldPtsFrq` = REPLACE( `fldPtsFrq` , '\\\\r' , '' ), `fldPtsAmp` = REPLACE( `fldPtsAmp` , '\\\\r' , '' ) WHERE `fldTemplateName` = '", 
                   names(template.L), "'", sep = "")
    lapply(query, function(x) RODBC::sqlQuery(dbCon, x))
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

dbUploadTemplate(templates = bithTemps,
                 uid = "root",
                 pwd = "RSW4xN!!",
                 db.name = "noh",
                 analyst = 1, 
                 locationID = "2",
                 date.recorded = "2017/09/07",
                 recording.equip = "Unknown",
                 species.code = "BITH",
                 type = "COR")
