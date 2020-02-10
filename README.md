# Project Plebiscite

Using the Census & neural nets to learn more about voting behavior.

## Description & Questions
This is a project I completed as an undergraduate. I combined US Census data at the block level with individual entries in the voter file, and used regression and an MLP neural net to investigate two concepts:
1. Assortative habitation. Are people more likely to vote if their neighbors vote? Do the characteristics of a set of neighbors help to understand the voting behavior of a group?
2. Prediciton of turnout. Can we better predict if someone is likely to vote if we know things about their neighbors, or their  demographics at an aggregate level?
I chose to use Philadelphia for the simple reason that I had lived there for 22 years and already had a copy of the voter file, with results from 2016. 

## Process Notes
- The part of this project that actually took the most time was matching each address to a census block. I wrote a whole set of routines, and while I assume the biggest number is the latest version, I'm not certain. The important thing to know is that I use the Census Master Address File to try to match each address within a block, then fall back to an API from a random website paired with the FCC or the Census Bureau API.
- I tried to train a neural net with 1 million line dataset on a 9-year-old MacBook, so this project was submitted almost a week late after I successfully implemented H2O on a 16-core AWS instance. There is code somewhere in this repository to train both in an R session and using H2O, pick your poison.

## Outcomes
My conclusions were that we can better predict whether an individual voted in the 2016 election by incorporating block-level demographic data (slightly better with a logistic regression, much better with a neural net). This suggested to me that people behave similarly to their neighbors, but it was beyond the scope (and deadline!) of the project to do an additional analysis. At the very least, we can predict individual behavior using aggregate data.

[Read the full report here.](Report/Report.pdf)

## Replicability
This is seriously amateur code, before I had read & adopted a style guide. Replicate at your own risk -- but be warned, this repository includes none of the raw data in .csv or .shp formats. (There are two reasons: one, they're too large for GitHub; two, I'm not sure I'm allowed to repost them.) You may be able to use some of the .Rdata objects instead, but I can't promise everything is included. 

If you have any questions, or if it is somehow important for you to replicate this project, please don't hesitate to reach out.
