## This file is designed to help you implement the two-regression algorithms.  The first 50 lines are for
## reading, to get you oriented to what's happening, and the subsequent lines constitute the actual demo.
## You can run this file by clicking the Source button at the top right of this window, by pressing
## ctrl+shift+enter, or by selecting all the text then clicking run (or pressing ctrl+enter)

## Now, here is the necessary background before you run the code:

## To process one monitor, you first have to provide five pieces of information: 1. a RAW.csv file (primary
## accelerometer); 2. an IMU.csv file (gyroscope and magnetometer) 3. the wear location; 4. the subject ID.
## 5. which algorithm(s) you want to predict with.

## The IMU.csv file isn't necessary if you're going to make predictions with Algorithm 1
## (accelerometer-only).

## The five pieces of information are fed into a function that works behind the scenes to produce estimates
## using the algorithm(s) you specified.

## This demonstration file does the following for data from one monitor: 1. Manually identifies the five
## pieces of information 2. Plugs them into the function 3. stores the results in a csv file

## This can be extended to process data from multiple monitors. A full demonstration is beyond the scope of
## this file, but you would essentially need to use R's matrix-apply command to feed successive five-piece
## sets of information through the function and store them in a list.  To apply algorithms 2-3 to a bunch of
## monitors, it would look something like:

## TwoRegPredictions <- mapply(twoReg, RAW = RAW_files_vector, IMU = IMU_files_vector, Wear.Location =
## WearLocations_vector, PID = id_vector, Algorithm = c(2,3), SIMPLIFY = F)


# Where: RAW_files_vector is a vector of file paths to primary accelerometer data, IMU_files_vector is a
# vector of file paths to IMU data wearLocations_vector is a vector that defines the wear location of each
# monitor id_vector is a vector with the participant IDs to assign to each monitor It is absolutely
# essential that these four vectors correspond element-wise, e.g. the first entry of each vector provides
# the information for one monitor, the second entry of each vector provides the information for the next
# monitor, and so on.

## From there, you could use do.call(rbind, TwoRegPredictions) to combine all the predictions into one data
## set.


# ************************** BEGIN THE ACTUAL DEMO ************************# rm(list=ls()) source('Not Run
# -- Two Regression Processing FUNCTION.R') ## This is the file that has the two-regression predicting
# function source('Not Run -- ActiGraph Scripts.R') ## This file has helper functions for reading and
# formatting the data files source('Not Run -- Dates and Times.R') load('Algorithms.RData') ## This stores
# the information we're going to feed into the function. You could also input it directly into the
# function.  primary_file <- 'TestID_LW_RAW.csv' IMU_file <- 'TestID_LW_IMU.csv' attachment <- 'Left Wrist'
# ## Can choose from Hip, Left Wrist, Right Wrist, Left Ankle, or Right Ankle. Default is to predict for
# all five.  participant_NO <- 'TestID' Alg_to_use <- c(2,3) ## Now to implement the function testData <-
# twoReg_process(RAW = primary_file, IMU = IMU_file, Wear.Location = attachment, PID = participant_NO,
# Algorithm = Alg_to_use) ## Here's an example of how you might save the data data.table::fwrite(testData,
# file = 'Test Data Processed.csv', row.names = F)
