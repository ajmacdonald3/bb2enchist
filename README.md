# bb2enchist: Transform [BandedBirds.org](bandedbirds.org) Data into Encounter Histories

Amie MacDonald

ajmacdonald3@gmail.com

## Overview

Encounter histories are a common format for analyzing mark-resight data. Shorebird flag resighting data are often submitted to
[BandedBirds.org](bandedbirds.org), an international database of shorebird resightings along the Atlantic Flyway. However, data formatted
for submission to [BandedBirds.org](bandedbirds.org) are not arranged in encounter histories, nor is the format commonly used for analysis
in R. Especially when working with large datasets, automating the process of transforming resighting data submitted to
[BandedBirds.org](bandedbirds.org) into encounter histories can save time and reduce errors. `bb2enchist` provides functions that
facilitate this process.

## Installation

`bb2enchist` can be installed by installing the `devtools` package (if not already installed) and using the following code:

> `devtools::install_github("ajmacdonald3/bb2enchist")`

## Using bb2enchist

**Inputs:**
1. A CSV file of resighting data formatted exactly as submitted to [BandedBirds.org](bandedbirds.org)
2. A CSV file assigning each date within the study to a resighting period (optional - this is only necessary if setting custom
resighting periods)

**Outputs:**
1. A dataframe with encounter histories formatted for further analysis in R that can also be exported as a CSV file
2. A dataframe with encounter histories formatted for analysis in Program MARK that can be exported as an INP (text) file

### Functions

