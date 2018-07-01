read.AG <- function(file, skip = NULL, light = F){

  #The "light = T' argument isn't for light activity. It's a workaround in case a uniaxial file is used, in which
  #case vector magnitude can't be calculated. Basically "light = T" is for the adult 2RM (uniaxial), whereas
  #for youth files (or any other triaxial files) "light = F" will pick up all three axes

  cat('\n\n\n****Reading', basename(file))
  cat('... ')

  reqNames <- c("Date", "Time", "Axis1", "Axis2", "Axis3","Vector.Magnitude")
  if(light) reqNames <- c("Date", "Time", "Axis1")

  if(is.null(skip)){AG <- data.frame(data.table::fread(file, stringsAsFactors = F))
  } else{
                    AG <- data.frame(data.table::fread(file, stringsAsFactors = F, skip = skip))
                    if(sum(grepl('V[0-9]', names(AG)))==ncol(AG)){
                      AG <- data.frame(data.table::fread(file, stringsAsFactors = F, skip = skip-2))
                      AG <- setNames(AG[-1,], AG[1,])
                      AG[,grepl('axis', names(AG), ignore.case = T)] <- sapply(AG[,grepl('axis', names(AG), ignore.case = T)],
                                                                               function(x) as.numeric(x))
                    }
  }

  nameTest <-
    sum(!reqNames%in%names(AG))!=0

  if(nameTest){
    message(paste('Unable to complete processing because the following variables are missing from the csv file:',
               paste(c("","Date", "Time", "Axis1", "Axis2", "Axis3","Vector.Magnitude")[!c("","Date", "Time", "Axis1", "Axis2", "Axis3","Vector.Magnitude")%in%names(AG)], collapse = '\n  ')))
    message('Returning NULL')
    return(NULL)
  }

  reqNames <- names(AG)[names(AG)%in%c("Axis1", "Axis2", "Axis3","Vector.Magnitude")]
  AG$DateTime <- as.POSIXlt(paste(AG$Date, AG$Time), format = '%m/%d/%Y %H:%M:%S')
  AG$dayofyear <- get.dayofyear(AG$Date)
  AG$minofday  <- get.minofday.int(AG$DateTime)
  AG$FileID <- basename(file)
  cat('Done!')
  return(AG[,c('FileID', 'Date', 'Time', 'DateTime', 'dayofyear','minofday',reqNames)])
}

cwr <- function(AG){
  0.0137 * exp(0.848 * (log(AG)))
}

ila <- function(AG){
  1.219 - (0.145 * (log(AG))) - (0.0586 * (log(AG))^2) + (0.0229 * (log(AG))^3)
}

adult.cwr <- function(AG){
  2.2924275 * exp(0.00084679 * (AG))
}

adult.ila <- function(AG){
  0.749395 + (0.716431 * (log(AG))) - (0.179874 * (log(AG))^2) + (0.033173 * (log(AG))^3)
}

youth.VM2RM <- function(file, skip = NULL){

  if(is.null(skip)) AG <- read.AG(file) else AG <- read.AG(file, skip = skip)
  if(is.null(AG)) return(NULL)

  cat('\nCalculating 10s epochs...')
    tenSec <- lapply(with(AG, list(Axis1, Axis2, Axis3)),function(AG){
      tapply(AG, cumsum(seq_len(length(AG))%%10==1), sum)})
    tenSec <- setNames(data.frame(do.call(cbind, tenSec)), c('Axis1','Axis2','Axis3'))
    tenSec$Vector.Magnitude <- apply(tenSec, 1, function(x) sqrt(sum(x^2)))

    tenSec <-
      cbind(subset(AG[seq_len(nrow(AG))%%10==1,], select = -c(Axis1, Axis2, Axis3, Vector.Magnitude)),
            tenSec)[1:floor(nrow(AG)/10),]
  cat(' Done!\n')

  cat('Calculating 60s epochs...')
    sixtySec <- lapply(with(AG, list(Axis1, Axis2, Axis3)),function(AG){
      tapply(AG, cumsum(seq_len(length(AG))%%60==1), sum)})
    sixtySec <- setNames(data.frame(do.call(cbind, sixtySec)), c('Axis1','Axis2','Axis3'))
    sixtySec$Vector.Magnitude <- apply(sixtySec, 1, function(x) sqrt(sum(x^2)))

    sixtySec <-
    cbind(subset(AG[seq_len(nrow(AG))%%60==1,], select = -c(Axis1, Axis2, Axis3, Vector.Magnitude)),
          sixtySec)[1:floor(nrow(AG)/60),]
  cat(' Done!\n')

  cv10s <- get.cvPER(tenSec$Vector.Magnitude)
  modelSelect <- as.character(cut(cv10s, c(-Inf, 25, Inf), c('CWR', 'Lifestyle')))

  METRMR10s <- ifelse(tenSec$Vector.Magnitude<=75, 1,
                      ifelse(modelSelect=='CWR', cwr(tenSec$Vector.Magnitude),
                             ila(tenSec$Vector.Magnitude)))

  if(length(METRMR10s)!=nrow(tenSec)) tenSec <- tenSec[seq_along(METRMR10s),]

  tenSec <- within(tenSec, {METs = METRMR10s})

  sixtySec <- within(sixtySec, {METs = tapply(tenSec$METs,
                                              cumsum(seq_len(nrow(tenSec))%%6==1),
                                              mean)[seq_along(sixtySec$Vector.Magnitude)]})

  AGday <- split(sixtySec, sixtySec$dayofyear)
  AGday <-
    do.call(rbind,
    lapply(AGday, function(x){
      FileID = x$FileID[1]
      Date = x$Date[1]
      MeasuredTime = paste(x$Time[1], x$Time[nrow(x)], sep = ' - ')
      MeasuredMins = max(x$minofday) - min(x$minofday)
      MeasuredHrs = MeasuredMins/60
      dayofyear = x$dayofyear[1]
      meanVM = mean(x$Vector.Magnitude)
      mean_METs = mean(x$METs)
      SEDmins = sum(x$METs<=1.5)
      LPAmins = sum(x$METs>1.5 & x$METs<=4)
      MPAmins = sum(x$METs>4 & x$METs<=6)
      VPAmins = sum(x$METs>6)
      MVPAmins = sum(MPAmins, VPAmins)

      return(data.frame(FileID, Date, MeasuredTime, MeasuredMins, MeasuredHrs,
                        dayofyear, meanVM, mean_METs, SEDmins, LPAmins, MPAmins,
                        VPAmins, MVPAmins))
    })
    )

  frames <- list(tenSec = tenSec, sixtySec = sixtySec, dayByDay = AGday)
  basicName <- unlist(strsplit(basename(file), '\\.'))[1]

  cat('\nWriting 10s csv...')
  tenSec <- data.frame(sapply(tenSec, as.character))
  data.table::fwrite(tenSec, file = file.path(dirname(file), 'Individual R Output/10-s',
                                              paste(basicName, '-- 10s_Crouter.csv')),
                     row.names = F)
  cat(' Done!')

  cat('\nWriting 60s csv...')
  sixtySec <- data.frame(sapply(sixtySec, as.character))
  data.table::fwrite(sixtySec, file = file.path(dirname(file), 'Individual R Output/60-s',
                                                paste(basicName, '-- 60s_Crouter.csv')),
                     row.names = F)
  cat(' Done!')

  cat('\nWriting daily summaries csv...')
  data.table::fwrite(AGday, file = file.path(dirname(file), 'Individual R Output/Daily Summaries',
                                             paste(basicName, '-- daily_Crouter.csv')),
                     row.names = F)
  cat(' Done!')

  return(frames)
}

adult.2RM <- function(file, skip = NULL, light = T){

  #The "light = T' argument isn't for light activity. It's a workaround in case a uniaxial file is used, in which
  #case vector magnitude can't be calculated. All that's necessary for this 2RM is the vertical axis, so if "light"
  #is indicated, it just means that the code will only really care about finding the vertical axis, i.e. a lighter set
  #of variables to look for. Probably not named the best, but it is what it is.

  if(is.null(skip)) AG <- read.AG(file, light = light) else AG <- read.AG(file, skip = skip, light = light)
  if(is.null(AG)) return(NULL)
  epoch = as.numeric(diff.POSIXt(AG$DateTime)[1])

  if(!epoch%in%c(1,10)){
    message('Epoch length needs to be 1-s or 10-s for this version of the code.\nReturning NULL.')
    return(NULL)
  }

  Names_Reint <-
    paste(c('Axis1', 'Axis2', 'Axis3')[c('Axis1', 'Axis2', 'Axis3')%in%names(AG)], collapse = ',')

  if(epoch==10){
    cat('\nDetected that epoch length is 10-s. No initial reintegration necessary.')
    tenSec <- AG
    } else{
      cat('\nCalculating 10s epochs...')
      tenSec <- lapply(with(AG, eval(parse(text = paste('list(', Names_Reint, ')', sep = '')))),
                       function(AG){
        tapply(AG, cumsum(seq_len(length(AG))%%10==1), sum)})
      tenSec <- setNames(data.frame(do.call(cbind, tenSec)), unlist(strsplit(Names_Reint, ',')))
      if(length(setdiff(c('Axis1', 'Axis2', 'Axis3'), Names_Reint))==0) tenSec$Vector.Magnitude <-
                                                                                apply(tenSec, 1, function(x) sqrt(sum(x^2)))

      tenSec <-
        cbind(subset(AG[seq_len(nrow(AG))%%10==1,], select = eval(parse(text = paste('-c(', Names_Reint, ')', sep = '')))),
              tenSec)[1:floor(nrow(AG)/10),]
      cat(' Done!')
  }

  cat('\nCalculating 60s epochs...')
  sixtySec <- lapply(with(tenSec, eval(parse(text = paste('list(', Names_Reint, ')', sep = '')))),function(AG){
    tapply(AG, cumsum(seq_len(length(AG))%%6==1), sum)})
  sixtySec <- setNames(data.frame(do.call(cbind, sixtySec)), unlist(strsplit(Names_Reint, ',')))
  if(length(setdiff(c('Axis1', 'Axis2', 'Axis3'), Names_Reint))==0) sixtySec$Vector.Magnitude <-
                                                                            apply(sixtySec, 1, function(x) sqrt(sum(x^2)))

  sixtySec <-
    cbind(subset(tenSec[seq_len(nrow(tenSec))%%6==1,], select = eval(parse(text = paste('-c(', Names_Reint, ')', sep = '')))),
          sixtySec)[1:floor(nrow(tenSec)/6),]
  cat(' Done!\n')

  if(sum(is.na(AG))>0){
    message(paste('Removing',
                  sum(is.na(AG)),
                  'rows of missing data. This may affect things, so you should follow up with manual inspection.'))
    AG <- na.omit(AG)
    tenSec <- na.omit(tenSec)
    sixtySec <- na.omit(sixtySec)
  }

  cv10s <- get.adult.cvPER(tenSec$Axis1)
  modelSelect <- as.character(cut(cv10s, c(-Inf, 10, Inf), c('CWR', 'Lifestyle')))

  METRMR10s <- ifelse(tenSec$Axis1<=8, 1,
                      ifelse(modelSelect=='CWR', adult.cwr(tenSec$Axis1), adult.ila(tenSec$Axis1)))

  if(length(METRMR10s)!=nrow(tenSec)) tenSec <- tenSec[seq_along(METRMR10s),]

  tenSec$METs <- METRMR10s

  sixtySec$METs <- with(sixtySec, tapply(tenSec$METs,
                                              cumsum(seq_len(nrow(tenSec))%%6==1),
                                              mean)[seq_along(sixtySec$Axis1)])

  AGday <- split(sixtySec, sixtySec$dayofyear)
  AGday <-
    do.call(rbind,
            lapply(AGday, function(x){
              FileID = x$FileID[1]
              Date = x$Date[1]
              MeasuredTime = paste(x$Time[1], x$Time[nrow(x)], sep = ' - ')
              MeasuredMins = max(x$minofday) - min(x$minofday)
              MeasuredHrs = MeasuredMins/60
              dayofyear = x$dayofyear[1]
              meanVA = mean(x$Axis1)
              mean_METs = mean(x$METs)
              SEDmins = sum(x$METs<=1.5)
              LPAmins = sum(x$METs>1.5 & x$METs<=3)
              MPAmins = sum(x$METs>3 & x$METs<=6)
              VPAmins = sum(x$METs>6)
              MVPAmins = sum(MPAmins, VPAmins)

              return(data.frame(FileID, Date, MeasuredTime, MeasuredMins, MeasuredHrs,
                                dayofyear, meanVA, mean_METs, SEDmins, LPAmins, MPAmins,
                                VPAmins, MVPAmins))
            })
    )

  frames <- list(tenSec = tenSec, sixtySec = sixtySec, dayByDay = AGday)
  basicName <- unlist(strsplit(basename(file), '\\.'))[1]

  cat('\nWriting 10s csv...')
  tenSec <- data.frame(sapply(tenSec, as.character))
  data.table::fwrite(tenSec, file = file.path(dirname(file), 'Individual R Output/10-s',
                                              paste(basicName, '-- 10s_Crouter.csv')),
                     row.names = F)
  cat(' Done!')

  cat('\nWriting 60s csv...')
  sixtySec <- data.frame(sapply(sixtySec, as.character))
  data.table::fwrite(sixtySec, file = file.path(dirname(file), 'Individual R Output/60-s',
                                                paste(basicName, '-- 60s_Crouter.csv')),
                     row.names = F)
  cat(' Done!')

  cat('\nWriting daily summaries csv...')
  data.table::fwrite(AGday, file = file.path(dirname(file), 'Individual R Output/Daily Summaries',
                                             paste(basicName, '-- daily_Crouter.csv')),
                     row.names = F)
  cat(' Done!')

  return(frames)
}
