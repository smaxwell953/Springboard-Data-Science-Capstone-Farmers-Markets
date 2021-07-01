# Springboard-Data-Science-Capstone-Farmers-Markets
<h1>My Springboard Capstone project on farmer's markets</h1>
<h2>1. Introduction</h2>
<p>Farmer's markets are a key factor in the health of a population due to greater proximity to quality food sources and greater trust from consumers because they can get to know their local farmers and trust that the food is produced to their standards.  With news stories of food recalls due to pathogens or foreign matter, farmer's markets can be seen as a source of food produced closer to home, with less risk of contamination. Farmer's markets that sell a greater variety of foods and accept non-cash methods of payment may be likelier to draw more customers and increase their presence in the local areas.</p>

<h2>2. Data Collection</h2>
<p>The data comes from the Food Environment Atlas data, updated March 27, 2018.  This dataset was compiled by the United States Department of Agriculture (USDA). This dataset contains 3,143 records, one for each county or county-equivalent in the United States, and 14 columns. https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/.</p>

<h2>3. Data Wrangling</h2>
<p>After loading the data, columns that did not add value to the analysis were removed. These included columns with data not relevant to farmer's markets, the total number of markets in each county (as a change in this number is more vague in measuring the growth or decline), the percentages of each kind of market, and the percent change from 2009 to 2016 (as a percent increase from zero would produce invalid data).</p>

<h2>4. Exploratory Data Analysis</h2>
<p>I calculated the percentage of counties that saw an increase in farmer's markets, as well as the percentages of markets in that accepted all forms of payment and markets that sold all food types. I also calculated the percentage of counties in each region (Northeast, Midwest, South, West) that saw an increase in markets per 1,000 people. I plotted histograms comparing the counties that saw an increase in markets with all counties. Independent-sample t-tests showed that the hypotheses of the difference in means being equal to zero for all types of markets is not rejected. There is no significant difference between the means of all types of the markets in counties that saw increases and the means of the markets in all counties.</p>

<h2>5. Machine Learning</h2>
<p>Linear regression showed that each type of food sold has stronger correlations with an increase in farmer's markets per 1,000 people than all food sold combined. Each type of payment method accepted also had stronger correlations than all payment types combined.</p>

<h2>6. Final Report</h2>
<p>This notebook contains my complete analysis and results.</p>
