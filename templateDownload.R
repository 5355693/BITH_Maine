## The out-of-the-box version of RODBC has a 65535-byte limit on transfers.
## For these giant concatenated columns in the tblTemplate, this ends up cutting 
## off part of the field. 
## To solve this, I had to fork the RODBC repo, comment out the offending line (it was line 741)
## and then reinstall my forked version using devtools. Even with this change, the buffer 
## was too small so I had to tell RODBC to only download 1 row at a time.

## Use this after forking and changing RODBC:
###devtools::install_github("5355693/RODBC")

bith <- dbDownloadTemplate(db.name = "noh", uid = "root", pwd = "RSW4xN!!", type = "COR",
                   species = "BITH", rows_at_time = 1)
list.files(path = "/Volumes/Untitled")
viewSpec(clip = "/Volumes/Untitled/W33L13_2017-06-05_205800_EDT.wav", 
         interactive = TRUE, frq.lim = c(1,15), spec.col = gray.2(),
         annotate = TRUE)

cscores <-corMatch(survey = "/Volumes/Untitled/W33L13_2017-06-05_205800_EDT.wav",
                   templates = bith,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cscores 
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects, ask = FALSE, t.each = 300)
getDetections(cdetects)


