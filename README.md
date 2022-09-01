# appeals_monthly_SR
`appeals_monthly_SR` is a proof of concept automation of appeals monthly statistical release. It requires the user to link to a csv file containing the latest appeals data (covering the last 12 month period).

## Pre-requisites
* R
* RStudio/IDE
* csv files containing appeals data covering the last 12 month period

## What's in this repo
`PINS_monthly_SR.Rmd` - main rmarkdown file that when run produces `PINS_monthly_SR.html`
`PINS_monthly_SR_workings.R` - script called by `PINS_monthly_SR.Rmd` to produce figures/tables/charts for .html
`PINS_monthly_SR.html` - example output .html file. Note: this is not styled as this is done on the Whitehall upload site.

## Running

* Download repository / clone
* Open `PINS_monthly_SR.Rmd` in RStudio/IDE and update the following variables:

* `publication_date` - this should be the date statistical release will be published, e.g. "21/07/2022" provided as string in the format "dd/mm/yyyy"

* `appeals_data` - this should be a link to the latest/relevant appeals data, e.g. "C:/Users/GerulaitisJoanna/OneDrive - Planning Inspectorate/Admin/Data_Analysis/to_june_2022_appeals.csv"

## Future developments
* This suite currently only automates the first portion of the standard monthly statistical release (is incomplete). It is a proof of concept that can be added to further or adapted.
* Some adjustments may need to be made to make sure the output .html is in the right format for upload on Whitehall (GOV.UK).
* Dummy data has been used, this will need to be the correct appeals data for a production version.
