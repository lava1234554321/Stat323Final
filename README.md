Alice Ni 
Lavanya Sunder

# Stat323Final
Stat323 Final Project. Working with Yelp Review Data, and performing sentiment analysis and other NLP to un-bias reviews within Yelp. Final project includes a Shiny application that will allow the user to analyze the biases present in the reviews for certain restaurants in the data set. 

##Instructions
Run `Final_Project.Rmd` to create files `all.csv`, `groups.csv`, and `model_coefficients.html`. (Note, this will take a long time, approximately 16 hours). `Final_Project.Rmd` uses files stored in gort (paths are specified). 
Run `Shiny.Rmd` after, which uses those files created in `Final_Project.Rmd`, to run the shiny application.

##Files
* `Final_Project.Rmd` contains the writeup and the code of our sentiment analysis and building of mixed models.
* `all.csv` includes all of the restaurant attributes, including sentiment score and newly predicted ratings using the models. 
* `groups.csv` contains the information per restaurant of number of reviews in each sentiment cluster. 
* `model_coefficients.html` contains a nicely formatted table containing information on our 5 mixed effect models (one per sentiment cluster).
* `Shiny.Rmd` contains the ui and server for running a shiny app using `all.csv`, `groups.csv`, and `model_coefficients.html`
