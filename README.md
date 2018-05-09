---
title: Great Lakes Elections Project 2016 (MIT Election Data and Science Lab)
author: Michael DeCrescenzo
---

# About this repository

Contains my (Michael DeCrescenzo's) code contributing to the Great Lakes Elections Project (Joyce Foundation/MIT Election Admin data project)

This project is currently a work in progress


# Building the data

This repository contains R source code to process data from the EAVS, CCES, SPAE, and so on.

Eventually there will be a single R file that controls the production of all results


# Repository notes for Evan

## Data files and zips:

One issue with Git is the handling of data files. Github doesn't like huge data files, but we're working with databases that are distributed with `.zip` or `.gz` files anyway, so code contains tricks to work with that.

The repository pushes the compressed databases online. When offline, the code simply unzips the database and then reads it into software, rather than read in any huge, raw CSV files directly. This lets us avoid uploading huge CSVs to the online repository.


# Other Notes

see `mit-notes.md` for more detailed notes, to-do lists, etc.
