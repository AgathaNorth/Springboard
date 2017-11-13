This project is adapted from Harvard University's statistical software workshop.

http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html

These statistical software workshop materials by Harvard University are licensed 
under a Creative Commons Attribution-ShareAlike 4.0 International License.




First we read in all the data that will be used in these exercises, and then examine what we have to work with. Exercise 0 wants us to fit the data using linear regression, and to add other predictors to see if they improve the model. 

I made a subset of the data using just metro and energy, as desired by the exercise, and then began to add more predictors and slowly removed them until my final model used metro, pop, density, and waste. Adding these predictors seemed to weaken the model, reducing the adjusted R squared from the more simple iteration.

In exercise 1 we add an interaction shows instant improvement over the previous model, and then again adding regions which greatly improves the model.