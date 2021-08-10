# Domain of influence Analysis: Tsyganenko

__code author:__ Brecht Laperre  
__supervisor:__ Giovanni Lapenta  
__collaborator:__ Maria-Elena Innocenti

## Publication

This code has been used to perform the experiments for the section 'Tsyganenko' in the published paper

[__Domain of Influence analysis: implications for Data Assimilation in space weather forecasting__](https://www.frontiersin.org/articles/10.3389/fspas.2020.571286/full)

__authors__:   
Dimitrios Millas,  
Maria Elena Innocenti,  
Brecht Laperre,  
Joachim Raeder,  
Stefaan Poedts and  
Giovanni Lapenta

The paper has been published in [__Frontiers in Astronomy and Space Sciences Stellar and Solar Physics__](https://www.frontiersin.org/journals/603).

## Instructions

Open the Makefile and specify which model will be used and the inputfile to be read by the Tsyganenko models. 
Possible models to choose are
* T89
* T96
* TS04
* TA15


The `INPUT` name should be the name of a file in `model/input`, without `.csv`. For example, if an inputfile named `input_Vx.csv` exists, the value for `INPUT` would be `input_Vx`. The ensemble will be saved in the `output` folder of the respective model. The ensemble is created by simply running
```
make ensemble
```
The domain of influence can then be generated by calling
```
make representer
```


