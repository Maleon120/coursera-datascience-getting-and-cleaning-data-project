

#Original questions

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# Tidied Data from the Human Activity Recognition Using Smartphones Dataset

A few steps were taken to transform the initial data set. The test and train sets have were merged and the subject identifiers and activity labels were pulled in to create a single data set. The activity identifiers were translated from identifiers into human-readable names. Only the mean and standard deviation variables were kept. Those variables were further summarized by taking their mean for each subject/activity pair. The data is in "wide" format as described by [Wickham](http://vita.had.co.nz/papers/tidy-data.pdf); there is a single row for each subject/activity pair, and a single column for each measurement.

The final data set can be found in the `tidyMeans.txt` file, which can be read into R with `read.table("tidyMeans.txt", header = TRUE)`. A detailed description of the variables can be found in `CodeBook.md`. The basic naming convention is:

  Mean{timeOrFreq}{measurement}{meanOrStd}{XYZ}

Where `timeOrFreq` is either Time or Frequency, indicating whether the measurement comes from the time or frequency domain, `measurement` is one of the original measurement features, `meanOrStd` is either Mean or StdDev, indicating whether the measurement was a mean or standard deviation variable, and `XYZ` is X, Y, or Z, indicating the axis along which the measurement was taken, or nothing, for magnitude measurements.