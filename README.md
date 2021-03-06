---
title: "Readme"
author: "Patryk Skowron"
date: "27.03.2015"
output: html_document
---

# Współzasiadanie w składach orzekających (Judging teams analysis application)

## General description

The application is designed to show judging teams distribution in Polish courts. Application is a part of *SAOS* project which is described on following websites:
+ https://github.com/CeON/saos (Website)
+ https://saos.icm.edu.pl/wiki/Strona_g%C5%82%C3%B3wna (Info)
+ https://github.com/saos-apps/saos (API)

User can select particular court from the database, see judges network and several statistics. Application in still under development.

# Input data

The input data for this application is data of polish judges and judgments. This app uses data downloaded through API and transformed to more convenient format. The input are following tables with corresponding variables:

* judgments
* judges
* divisions
* judges.net





|    |judgments    |judges       |divisions    |judges.net   |
|:---|:------------|:------------|:------------|:------------|
|V1  |judgmentID   |judgmentID   |CourtCode    |name1        |
|V2  |judgmentType |JudgeName    |CourtName    |name2        |
|V3  |judgmentDate |specialRoles |DivisionCode |Sex1         |
|V4  |judgmentYear |CourtCode    |DivisionName |Sex2         |
|V5  |CourtCode    |DivisionCode |NA           |judgmentID   |
|V6  |DivisionCode |judgmentDate |NA           |judgmentDate |
|V7  |NA           |judgmentYear |NA           |judgmentYear |
|V8  |NA           |JudgeSex     |NA           |specialRole1 |
|V9  |NA           |NA           |NA           |specialRole2 |
|V10 |NA           |NA           |NA           |CourtCode    |
|V11 |NA           |NA           |NA           |DivisionCode |

Description of each variable in above table is following:

* judgmentID: unique id of the judgment
* judgmentType: type of de judgment, i.e.: `DECISION/RESOLUTION/SENTENCE/REGULATION/REASONS`
* judgmentDate: date of the judgment, format `YY-MM-DD`  
* judgmentYear: year of the judgment, extracted from variable `judgmentYear`
* CourtCode: unique code of court
* DivisionCode: code of the division, unique inside each court
* JudgeName: Name of the judge. Some rows are empty or contain wrong data
* specialRoles: Special role assigned to judges
* JudgeSex: Sex of the judge, extracted from `JudgeName` so some rows are missing
* CourtName: official name of the court
* DivisionName: official name of the division/chamber in court
* name1: `JudgeName` for first judge. Used to build network of judges. Node of the network
* name2: `JudgeName` for second judge. Used to build network of judges. Node of the network
* Sex1: `JudgeSex`, additional attribute for `name1` node
* Sex2: `JudgeSex`, additional attribute for `name2` node
* specialRole1: `SpecialRolse`, additional attribute for `name1` node
* specialRole2: `SpecialRolse`, additional attribute for `name2` node

## Bookmarks

### Tests

Test bookmark.

### Net mark

In this bookmark user can see the network of judges in selected court. Each node (circle or traingle) refers to one judge. Link between 2 judges means that they have been in the same judging team minimum once. Each division in the court is marked with different colour. Circles refers to women and triangles to men. 

### Net pie

There the same network as in *Net mark* bookmark can be found. There is difference only with visualizing data. Each node is coloured with colour reffering to division(s) the judge is assigned to.

### Stats

In this bookmark some summarizing statistics can be found.

#### Plot: Number of judgments in following years

Plot shows number of judges in all divisions in specified court. Graph spans with years of judgments that are found in database for particular court. 

#### Plot: Number of judges in following years

Plot show numbe of unique judges working in all divisions in specified court. Year when judge works means that his/her name occured in minimum one judgment with spcified judgment date.

#### Histogram: fraction of female judges in judgment teams

Histogram reflects female judges preferences in particular case. This preference is calculated as follows:

$\frac{N_{female}}{N_{female}+N_{male}}$

so it is in range `{0,1}`. Red vertical line shows mean female preference for court.

#### Histogram: number of direct connections to other judges

Histogram shows number of direct connections for each judge in court. In the language of graphs this is node degree distribution. 

#### Histogram: number of times that two judges has been in the same judgment team

Histogram shows number of times that two judges in court have been in the same judging team. In the language of graphs this is histogram of weights of edges.

#### Plot: Maxium component relative size in following years

Plot shows what percentage of judges in court is included in maximum component of the network. Maximum component means that between all possible pairs of nodes (judges) in that component exists minimum one connection (through other judges and links between them).

#### Plot: Diversity in judging teams (rotation in judging teams)

Plot shows diversity in judging teams in particular court. This diversity is defined as follows:

$diversity=\frac{N_{links}}{N_{possible links}}$

where:

$N_{links}$ - number of unique links/edges between judges
$N_{possible links}$ - maximum number of links that could exist in court for this number of judges in court and configuration of judging teams (number of judges in each case). Points on the plot are divided into years when judgment was made.

### Top chart

Top chart show top 10 (or less if there is not enough in the court) judges in the number of cases they have been taking part.

## Functions description

### helpers.R functions

This file consists functions that are used to transform, analize and visualise the data. Those functions can be also used outside the shiny application. 

#### judges.top.c

Function takes data from `judges` table for specified court as an input. Then it creates a list of 10 (or less if not possible) judges that have the maximum number of cases in chosen court. This function is used to plot *Top chart* in application. Function returns data frame with columns `JudgeName` and `N.of.judgments`.

#### g.court

Function takes data from `judges` and `judges.net` tables fo specified court as an input. Function than creates a network of judges. This network is not simplified, i.e. there are multiple links between judges consisting information about judgments. The output of the function is network (graph).


#### g.simplify

Function takes network from `g.court` function as an input. Then it simplifies the network in following manner:
+ removes loops (node connects to itself)
+ removes multiple edges/links - the data assigned to those edges is concatenated and assigned to the "new" single edge.

Function returns simplified network (graph).

#### g.mark.matrix

Function takes simplified graph from `g.simplify` function as an input. Then it returns matrix with `TRUE/FALSE` values that specify whether or not each judge (node of the graph) belongs to division (each division is in new column). 

#### g.mark.list

Function takes simplified graph from `g.simplify` function and matrix from `g.mark.matrix` function as an input. Then, for each matrix column (each division in specified court) it creates the level of the list with id of nodes which belongs to this division. Then function applies different colour to each division as a name of the level. Colours are chosen from brewer pallete with alpha channel added (that some level of transparency to the node). Function returns a mentioned list. This list is used to colour groups in `Net mark` bookmark in shiny application.

#### g.color.pie

Function takes simplified network (graph) from `g.simplify` function as an input. Then the function assign a coloured pie chart for each node in given graph. Different colours correspond to different divisions. Pie chart is equally divided into parts. Function returns simplified graph with added pie chart.

#### j.coop.year

Function takes data from `judges` and `judges.net` table for specified court as an input. It also takes simplified graph from `g.simplify` function. Then the function calculates diversity of judging teams (described above) for each year in particular court. The output is data frame with `year` and `coop` columns.

#### judgm.year

Function takes data from `judges` table for specified court as an input. Then it calculates number of judgments for each year, corresponding to judgment date. Function returns data frame with columns `year` and `number.judgments`

judgm.year<-function(subset.judges.c){
  sub<-subset.judges.c[!duplicated(subset.judges.c$judgmentID),]
  df<-count(sub,"judgmentYear")
  names(df)<-c("year","number.judgments")
  df
}

#### j.year

Function takes data from `judges` table for specified court as an input.  Than it calculates numbe of judges for each year, corresponding to judgment date. Function returns data frame with columns 'year' and 'number.judges`.

#### max.comp

Function takes network(graph) from `g.court` for specified court as an input. Than the function calculates maximum component for that network for each year, corresponding to judgment date. Function returns data frame with columns `year` and `size.max.component`.

#### plot.sex.distribution

Function takes data from `judges` table as an input, but only rows where JudgeSex is assigned (`M/F` value). Then it calculates fraction $\frac{N_{female}}{N_{female}+N_{male}}$, where $N_{female}$ and $N_{male}$ is number of women and men, for each case. Additionally, the function calculates fraction of $\frac{N_{female}}{N_{female}+N_{male}}$ for whole court. The output is plot in format of ggplot package.

### funkcje.R functions

This script contains functions that help to compute/visualize particular things but have more general usage that this application.

#### multiplot

External function that takes list of ggplot plots as an input and arrange them in one multiplot with number of columns specified in `cols` parameter. The output is one multiplot graphics.

#### addalpha

External function that takes colours vector (color name / hexadecimal `#rrggbb` format / positive integer `i` meaning `palette()[i]`) and level of alpha (from `{0,1}` range) meaning the level of transparency as an input. Function returns vector of colours in RGB format with added alpha channel.

#### plog.legend

Function takes list of division names and colours (as level names) as input. Function creates graph and returns legend for the court divisions as a plot arranged in grid.

#### plog

Function takes network ( simplified graph) from `g.simplify` function, computed layout for the network and list of groups from `g.mark.list` function as an input. Drawing of network with marked divisions is an output.

#### plog.pie

Function takes network ( simplified graph) from `g.simplify` function, computed layout as an input. Then is prints out drawing of network with pie chart representing each node. Colours of the node corresponds with divisions judge is assigned to. Drawing of the network is an output of that function.

#### mycircle; mytriangle; mystar

Those functions add extra shapes to be plotted in network (graph) as nodes: circles, traingles and stars.

#### max.unique.links

Function that takes number of judges in court and array with number of judges in particular cases as an input. Then, the function calculates how many unique links (so sitting in the same judgment team) can be in such network with given parameters. The output is a number of maximum possible links in this network.

#### fun1

Function takes array (`array`) with number of judges in particular cases (in specified court), number of all judges (`njudges`) in court, array (`ordered`) of ordered elements from `array` and value (`nnext`) pointing to next element to be checked in `array` as an input. Then, recursively, function order elements from the `array` and returns it altogether with the next element.
