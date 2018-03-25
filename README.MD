README for Getting and Cleaning Data Course Project

The tidy.txt file
This file contains the averages(means) of the average and standard deviation data parameters. 
It is organized by the person who the data was collected on (the experimental "Subject") and the Activity type. 

The codebook.txt file
This file summarizes the fields shown in the tidy.txt file.

The run_analysis.R file
This file downloads the acceleration data collected and produces the tidy.txt file. It uses several libraries to reduce the amount of coding: (library(plyr), library(dplyr), library(reshape2)
1. It downloads the compressed data by ftp
2. Combines the test and training data into one file
3. Adds columns for the Subject and Activity
4. Selects only those datafields that are means or standard deviations
5. Assigns column headings after removing characters in order to make the headings more readable
6. Replaces the numerical Activity data witht the corresponding text
7. Melts and recasts the data to produce a table that is a mean of each of the mean and standard deviation values for a given person performing and activity.
8. Writes the resuling table to a file called tidy.txt
9. The function that runs the script is called acceldata()
