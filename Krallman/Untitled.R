# ###############################################################
#                                                               #
# Author: Brian Lambert, Minyue Wang, Mary Yu                   #
# Name: krallman_adminG2_code.R                                 #
# Description: .                                                #
#                                                               #
# setwd(“/Users/brianlambert/Desktop/STA404/krallman”)          #
#                                                               #
#################################################################


#========================== Data ==========================

basic_demographics = read.csv("/Users/brianlambert/Downloads/result/Basic demographics fall 17 ftft Oxford.csv", header = T)
act_composites = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/ACT Composite Test score dummy id.csv", header = T)
courses_enrolled = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Courses enrolled 15th day - dummy id.csv", header = T)
hs_rank_type= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/HS percentile rank and hs type.csv", header = T)
legacy= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Legacy dummy id.csv", header = T)
financial_need= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Level of financial need dummy id.csv", header = T)
parental_education = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Parental education dummy id.csv", header = T)



division = basic_demographics['Division']

ggplot(division, aes(x = as.factor(Division), fill = Division)) +
    geom_bar()
