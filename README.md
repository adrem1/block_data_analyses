# Cardano block data analyses
A collection of Koios queries and R scripts to analyse Cardano block data.

## Start here
In this repo you will find: 
- bash scripts to automate Koios queries used to pull required data 
- R scripts to replicate my analyses or modify them to ask different questions

## General considerations
i. The queries used to fetch all required datasets are provided here for independent 
data generation or checking. Most of the Koios queries are well within the free tier daily limits for Koios.

ii. In every analysis folder you'll find all queries and scripts needed for it.
Koios queries should be run first, to fetch data from Cardano. Then you can 
proceed with the analyses (either your own, or following mine).

iii. In the R scripts provided the expected directory structure is the following:

> YOUR-WORKING-DIRECTORY/data/DATAFILES

This can be modified if not suitable, but data import instructions will need to reflect the changes.

iv. I have conducted several separate analyses, and organised the required files accordingly.
For those interested in replicating or modifying these analyses, please don't hesitate to ask 
if you find how I organised this repo confusing.

## About the analyses
The results of the analyses are presented with commentary on the Cardano Forum:

[LINK](link)

I encourage interested people to: 
- re-run these analyses, to verify them
- provide code to run them in different languages (eg Python)
- ask their own questions

## Acknowledgements
I wish to thank Rich [ECP] and Wayne [OTG] for providing valuable insights during this work.
