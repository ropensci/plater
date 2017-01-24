---
name: plater-development
layout: post
title: From a million nested `ifelse`s to the plater package
date: 2017-01-25
authors:
  - name: Sean Hughes
    url: https://github.com/seaaan
categories:
  - blog
tags:
- ropensci
- packages
- plater
- R
---

In the lab, almost everything I do, I do in a microtiter plate. The problem was, it was an error-prone mess to work with in R (not to mention Excel). I started out putting one `ifelse` inside of the next to describe which well was which, put with the help of [R Packages](http://r-pkgs.had.co.nz/) and the [amazing](http://deanattali.com/) [reviewers](http://www.juliagustavsen.com/) and [editors](https://scottchamberlain.info/) at rOpenSci, I ended up with a package that makes it so easy you don't even notice. 

## Plates are great

Microtiter plates are essential in the lab. Basically an ice cube tray about the size of an index card, they have "wells" for between 6 and 384 ice cubes (up to 6144 if you're a robot!). Except instead of freezing water, you can use each well for a different sample or experimental condition. 

For example, say I have 8 samples and want to test 4 different drugs. Let's also say I want to test each drug on each sample three separate times. A 96-well plate is perfect for this: it's a grid of 12 columns and 8 rows. So each sample would go in its own row. Each drug would then go in a group of three columns, say Drug A in columns 1-3, Drug B in columns 4-6, and so on. 

Typically, I would make myself a map describing what goes where and take it with me into the lab to do the experiment. The map creates a powerful mental image binding each experimental condition to a particular physical location on the plate. With dramatic treatments, you might even be able to visually see the results of your experiment: all the cells in this column died, or the cells grew like crazy in that row.  

This is very space-efficient and convenient to work with physically and remember mentally.

## Plates are not tidy

The problem is that you can pack a ton of complexity into a small experiment and mapping that back into a [tidy](https://www.jstatsoft.org/article/view/v059i10) framework isn't always easy. Each well has an ID. The top left well is in row A and column 1, so its ID is A01. The well in the third row down and 5th column over is C05. But how do you say that everything in row A is sample X, everything in row B is sample Y, and so on? 

My first strategy was to put it in the code, with a mess like this: 

```
data <- dplyr::mutate(data, 
            Sample = ifelse(Row == "A", "Sample X", ifelse(
                Row == "B", "Sample Y", ifelse(
                    ...))), 
            Treatment = ifelse(Column %in% 1:3, "Drug A", ifelse(
                Column %in% 4:6, "Drug B", ifelse(
                    ...)))
            )
```

But doing this made me want to cry. 

My next strategy was to try and directly make a table and then merge it into the data. The table would look something like this: 

| WellId | Sample | Treatment | 
| ------ | ------ | --------- | 
| A01    | X      | Drug A    |  
| A02    | X      | Drug A    |     
| A03    | X      | Drug A    |
| A04    | X      | Drug B    |
| ...    | ...    | ...       |
| B01    | Y      | Drug A    |     
| B01    | Y      | Drug A    |     
| ...    | ...    | ...       |

While this merges nicely into a data frame and solves the problem of indicating what each well is, it's actually not that easy to create by hand, especially in more realistic experiments with more variables and a more complex plate layout. 

## `plater` to the rescue

The solution came from the plates themselves: store the data in the shape of the plate and then transform it into a tidy shape. Scientific instruments often provide data in the shape of a plate, in fact: you get back a spreadsheet with a grid of numbers shaped like your plate, with a cell for each well. 

My first step was to google around and figure out a convenient way of converting from one of those grids to a data frame with two columns: one of plate IDs and one of the numbers. 

So now I could take a `.csv` file with plate-shaped data and convert it into tidy form and connect it with the well ID. It didn't take long for me to start creating `.csv` files with sample and treatment information as well and then merging the data frames together. Now I could create plate maps really easily because they looked just like the plate I did the experiment in. 

With time and feedback from others, I experimented and refined the system. Instead of creating separate files for each variable (treatment, sample, data, ...), everything could go in one `.csv` file, with sequential plate layouts for each variable. I started calling this `plater` format and storing all of my data that way. 

## Is this thing good?