---
title: 'plater: Read, Tidy, and Display Data from Microtiter Plates'
tags:
 - data import
 - R
authors:
- name: Sean M Hughes
  orcid: 0000-0002-9409-9405
  affiliation: University of Washington
date: 27 September 2016
bibliography: paper.bib
---

# Summary

plater is an R [@R] package that makes it easy to work with data from experiments performed in microtiter plates.

Many scientific instruments (such as plate readers and qPCR machines) produce data in tabular form that mimics a microtiter plate: each cell corresponds to a well as physically laid out on the plate. For experiments like this, it's often easiest to keep records of what was what (control vs. treatment, concentration, sample type, etc.) in a similar plate layout form. 

plater defines a simple, plate-shaped file format for data storage, so it's easy to remember the experimental design, and provides functions to seamlessly convert between that format and a tidy [@tidy] data frame that's optimal for analysis. When the instrument produces data that's already tidy, plater helps combine that data with plate-shaped experimental metadata. Once the data is tidy, it's sometimes useful to look back at it in plate shape, so plater makes that easy, too. 

# References
