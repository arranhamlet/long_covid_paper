# Stochastic mathematical model for the estimation of long COVID in Washington state

Here we present our framework for the estimation of long COVID in Washington state through the use of a stochastic mathematical model coded in [odin](https://mrc-ide.github.io/odin/index.html) and executed through the programming language [R](https://www.r-project.org/). Though this example has been created using data for Washington state, and the data we have available, the code is flexible and can adapt to the information you have available if correctly specified.

There are three main sections to using the model in this walkthrough, all found within the R folder.
1) Fitting the model to data
2) Running the model
3) Plotting results

# Fitting the model


# Model description

![Model diagram showing the progression between states](img/diagram.png)
Figure 1. Model diagram showing the progression between states. Non-hospitalized cases (longcase) and Hospitalized cases (longhosp) enter the acute long COVID compartments LNH¬ and LH respectively. These progress to either recovered, R, or unrecovered, U with the probability αNH and αH or βNH or βH respectively.

Here we use a stochastic compartmental model to estimate the burden of long COVID. Each compartment contains 8 age categories, 5 race/ethnicity groups, 3 vaccination categories and 2 sexes, as well as an unknown category for missing information for each group. This results in 162 sub compartments per compartment. The number of sub-compartments within each compartment is due to the available data on cases and hospitalizations, and probabilities of long COVID. 

Long COVID cases in non-hospitalized individuals are derived by taking reported and unreported cases that occurred 3 months previously, and applying the probability of developing long COVID, α¬NH. Long COVID cases in hospitalized individuals follow the same process, entering the Long COVID compartment with the probability of developing long COVID as αH.
Transition out of this compartment occurs at rate λNH and λH for non-hospitalized and hospitalized. Progression to either Recovered or Long-term burden occurs at 1- θNH and θNH¬ and 1- θH and θH¬ for the non-hospitalized and hospitalized. 
Model equations
(dL_ijklm^NH)/dt=〖longcase〗_ijklm- L_ijkl^NH (〖bd〗_ij+ α_ijkl^NH+ β_ijkl^NH )
Where i is the 9 age groups, j the 3 sec categories, k 6 race/ethnicity groups, l the 4 vaccination categories, and m the 40 counties. The variable longcase is defined as:
〖〖〖longcase〗_ijklm=ρ〗_ijklm^NH cases〗_ijklm.
ρNH is the probability of a case developing COVID, and cases the number of cases. All sampling occurs using a binomial distribution. The background death rate, 〖bd〗_ij, those recovering, α_ijkl^NH, to the R compartment and those moving to the unrecovered compartment U, β_ijkl^NH.
(dL_ijklm^H)/dt=〖longhosp 〗_ijklm- L_ijklm^H (〖bd〗_ij+ α_ijkl^H+ β_ijkl^NH )
Where longhosp is:
〖〖〖longhosp 〗_ijklm= ρ〗_ijklm^NH hospitalizations 〗_ijklm
and ρH is the probability of a case developing COVID, and hospitalizations the number of hospitalizations. The background death rate, 〖bd〗_ij, those recovering, α_ijkl^NH, to the R compartment and those moving to the unrecovered compartment U, β_ijkl^NH,
〖dU〗_ijklm/dt= 〖L_ijklm^NH β〗_ijkl^NH+ 〖L_ijklm^H β〗_ijkl^H- U_ijklm (〖bd〗_ij+ γ),
Where γ, is the recovery rate from U to R.
〖dR〗_ijklm/dt= dL_ijklm^NH α_ijkl^NH+ dL_ijklm^H α_ijkl^H+U_ijklm γ- 〖dR〗_ijklm 〖bd〗_ij.
