## R Data Science Projects
![MTU Logo](/1/data/MTU_Logo.jpg)


#### Project 1 : ETL and analysis using dataframes on Person's Health Dataset.

<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/1/STAT8010%20Assignement%201%202020%20(HDip).pdf">Project Specification</a></li>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/1/data/assignment1.xlsx">Dataset</a></li>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/1/Bernard_McNamee_R00207204.R">Project Code</a></li>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/1/Bernard_McNamee_R00207204%20.rmd">R Markdown Code</a></li>
<br><br>
<p><b>Project 2 : Build Shiny app dashboard to display range of plots + Apply Monte Carlo simulation models</b></p>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/2/STAT8010%20Assignment%202_2020%20(HDip).pdf">Project Specification</a></li>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/2/data/process_sim.csv">Dataset</a></li>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/2/Bernard_McNamee_R00207204_Shiny.R">Project Code</a></li>
<li><a href="https://github.com/bjmcnamee/R_ETL_MTU_Assignments/blob/main/2/Bernard_McNamee_R00207204_Report%20.rmd">R Markdown Code</a></li>
<br>
- Build Shiny app dashboard to display range of plots for any combination of variables of the Pharam Process Dataset
- Fit LR to plot
- Apply two Monte Carlo simulation models to predict variable tconc for variable year 2015
<br><br><i>Consider a machine that inserts a needle into test tubes on a conveyer for sampling in a factory process. This machine may become misaligned in the 2 dimensions of the plane of conveyer travel (x and y axes) independently. The machine is realigned to centre at the start of each day and it then samples 200 test tubes throughout the day. The machine fails to sample correctly if it is misaligned in any direction by 2cm or more, as it misses the test tube (possibly colliding with the glass). The x misalignment is 0.1mm on average in the direction of conveyor travel (positive x-direction) for each test, but that this can vary somewhat with a standard deviation of 0.1mm. Similarly, the y misalignment is biased in the negative y-direction, and is much smaller on average; the engineers believe that the average misalignment in the negative y direction is 0.05mm per test, with a standard deviation of 0.05mm.</i><br><br>
- Simulate the distribution of misalignments at the end of the day
- Estimate the likelihood of failure throughout the day? 
- Visualise the simulated alignments of the machine at the end of the day on a scatterplot, showing the 2cm limit
<br><br><i>It costs €50,000 when the machine goes offline due to excessive misalignment and no further batches can be tested for the reminder of the day. Each batch passed through the machine results in gross profit of €400. If a batch is ready for testing but the machine is offline, there is a €500 cost for storage and alternate testing of each untested batch under the target number of tests per day.</i><br><br>
- Apply two Monte Carlo simulations to find the best strategy - i.e. what is the optimal target number of runs per day before realignment should be done.
