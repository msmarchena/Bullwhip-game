# bullwhipgame

The bullwhipgame is an educational game that has as purpose the illustration and exploration of 
the *bullwhip effect*, i.e. the increase in demand variability along the supply chain.


The game simulates the distribution process of a single product that uses a four stages supply chain: 
reailer, wholesaler, distributor and factory. The members of the supply chain need to
meet customer demand with minimal shortage situations and inventory cost, while satisfying service level requirements. All
participants use the same inventory replenishment policy, forecast method, delivery lead time and service level.
Holding and shortage cost are fixed and information sharing and cooperation is not allowed. 

The goal of the game is to minimize the total inventory cost in the supply chain.




**Installation:**

To play the bullwhipgame locally you must have installed R, Shiny and the packages used in the server file.<br>
Copy the "app.R" file and "www" folder in the same directory on your machine.<br>
Open one of the files in RStudio and click the "runApp" button.

From Github, the following command will download and run the application

`shiny::runGitHub('bullwhipgame', 'msmarchena')`

Live demo of the bullwhipgame can be found here https://marchenamarlene.shinyapps.io/bullwhipgame/
