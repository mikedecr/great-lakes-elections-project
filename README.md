---
title: Great Lakes Elections Project 2016 (MIT Election Data and Science Lab)
author: Michael DeCrescenzo
---

# About this repository

Contains my (Michael DeCrescenzo's) code contributing to the Great Lakes Elections Project (Joyce Foundation/MIT Election Admin data project)

This project is currently a work in progress.


# Building the data

This repository contains R source code to process data from the EAVS, CCES, SPAE, and so on.

For more on the structure of the code, see `R/README.md`. 


## Data files and zips:

One issue with Git is the handling of data files. Github doesn't like huge data files, but we're working with databases that are distributed with `.zip` or `.gz` files anyway, so code contains tricks to unzip large files upon initial execution.

The largest data files are managed with `git-lfs` on my (Mike's) end, but the end user should not notice any hiccups with regard to large files (I think? See [here](https://git-lfs.github.com/)). If a user does, they should contact me at `decrescenzo [ampersand] wisc [period] edu`. 



# Other Notes

see `mit-notes.md` for more detailed notes, to-do lists, etc.
