# ReadMe

## Purpose
This folder is intended to contain CSV files with choice sets for use in the second portion of the CGT task.

## Experimental Details
In the first decision-making portion of this study, participants will make choices using a standard choice set (i.e. all participants face an identical choice set).

We will then briefly, efficiently, and approximately identify each participant's unique risk attitudes and choice consistency using a grid-search method within a certain parameter space.

Then, in the second decision-making portion of the study, participants will make decisions about a bespoke choiceset (**one of those listed here**), tailored to their specific pattern of risky decision-making such that each participant experiences a series of choices that can be categorized as relatively **easy** (with a subjective probability of risky-taking near 0 or near 1) or relatively **difficult** (with a subjective probability of risk-taking near 0.5).

## Method
This is accomplished by creating many possible choice sets, each for use by an individual with a particular value of **rho** (the risk attitude parameter) or **mu** (the choice consistency parameter). Both parameters are based on a straightforward gain-only prospect theory model of risky decision-making.

## File Identification
Choice sets are identified in the file name by the **index numbers** for the rho and mu values used to create those choices. See the accompanying R script which created these choices to connect _parameter values_ to _indices_.

Files are anticipated to be ~7.4KB in size each. 
