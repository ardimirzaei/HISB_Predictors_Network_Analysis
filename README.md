# Predictors of Health Information–Seeking Behavior: Systematic Literature Review and Network Analysis

Available at [JMIR](https://doi.org/10.2196/21680)

Please cite as:

Mirzaei A, Aslani P, Luca EJ, Schneider CR
Predictors of Health Information–Seeking Behavior: Systematic Literature Review and Network Analysis
J Med Internet Res 2021;23(7):e21680
doi: 10.2196/21680

## File Explanation

The `main.R` file contains all the steps to reproducing the work in the published paper above. 

The `HISB_Predictors_Coded.csv` file contains the list of the significant predictors. This file is loaded to build the network analysis. It also contains the information regarding theories. 

Finally, the `Functions_and_Codes.R` file contains the custom functions used to make the networks. 

## Buliding the Data Files

In `main.R`, the file can be downloaded straight from this github. You can aso download a local copy, and load it that way. It is important to oad the custom functions as it makes it run the code. 

Once you run the function to `Clean_and_Correct_Database`, then you need to `Produce_Edges` and `Produce_Nodes`. 

There was the ability to remove names to see connections between lower levels predictors rather than their super class (e.g. sociodemographic, health and internet). This may not work with abbreviated column names. The abbreviation list is in the custom functions and code, if it needs to be revered. Alternatively, you can pass a list of terms, and it will remove them from coding. 

You can specify a cutoff (e.g. 2008) and then create the edge and node list for thpse years. 
Once the edge aand node list has been made, you can then export it to a CSV to use in Gephi (version 0.9.2). 

## Generating the Graphs

The function `plot_networks` prints the network. I chose to save it to a PDF but you can also plot it in the window. The graphs plo using the Fruchterman-Reingold algorithm, with some adjustments so that the labels do not overlap. The nodelist contains the membership for each nde, however, these memberships are made according to the whole grpah, rather than from the cutoff points. That is, if the whole network (made with al years combined) has 3 membership groups, then throughout the years the node colorings are produced using those 3 memberships, even though one year may have a network that has more than 3 membership groups. 

## Extra outputs

The last part of the `main.R` produces the outputs needed for the paper and the spread of the theories. 



Notice any issues, please get in touch! 
