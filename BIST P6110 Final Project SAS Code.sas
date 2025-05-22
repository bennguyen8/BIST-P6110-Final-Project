/* Important note: Assume alpha = 0.05 for all hypothesis tests unless otherwise stated */

proc template;
	define style mystyle; * name the style;
	parent = styles.rtf; * inherit all features from the style RTF;
		* now choose which styles to override from RTF;
		class body / fontfamily = "Arial";
	end;
run;


ods rtf file = "/home/u63919905/BIST P6110 - Statistical Computing with SAS/Final Project/Final Project.rtf" style=mystyle;


/* Importing the dataset */
proc import out=cognition
	file='/home/u63919905/BIST P6110 - Statistical Computing with SAS/Final Project/cognition.xlsx'
	dbms=XLSX
	replace;
	getnames=YES;
RUN;

/* Question 2: Clinically, Aβ SUVR is often used to determine Aβ positivity (Aβ >1.15). 
Are APOE-ε4 carrier statuses or sex associated with Aβ positivity? */
/* Calculate the average AB SUVR levels in each lobe and make new columns for them */
data cognition;
set cognition;
	temporal_lobe = mean(temporal_l_lobe, temporal_r_lobe);
	parietal_lobe = mean(parietal_l_lobe, parietal_r_lobe);
	cingulate_lobe = mean(cingulate_l_lobe, cingulate_r_lobe);
	frontal_lobe = mean(frontal_l_lobe, frontal_r_lobe);

/* Calculate the global mean with the average AB levels in each lobe */
	global_mean_AB = mean(temporal_lobe, parietal_lobe, cingulate_lobe, frontal_lobe);
run;
/* Check the distribution for AB levels */
title "Distribution of AB levels in cognition dataset";
proc sgplot data = cognition;
histogram global_mean_AB / scale=count;
xaxis label="Global mean amyloid β (Aβ) SUVR";
run;

/* Note: Global mean amyloid β (Aβ) SUVR is right-skewed, as a result Aβ levels
should be turned into a categorical variable*/

data cognition;
set cognition;
	if global_mean_AB > 1.15 then AB_positive = 1;
	else AB_positive = 0;
run;

/* To determine if APOE-ε4 carrier statuses or sex is associated with Aβ positivity, we would perform a
chi-squared test for independence */
/* Create variable ε4_carrier with all possible combinations */
data cognition;
set cognition; /* Taking all 3 combinations into consideration */
	 if rs429358 = 'C:C' and rs7412 = 'C:C' then ε4_carrier = 1;
	 if rs429358 = 'C:T' and rs7412 = 'C:T' then ε4_carrier = 1;
	 if rs429358 = 'C:T' and rs7412 = 'C:C' then ε4_carrier = 1;
	 else ε4_carrier = 0;
run;


/* Check assumptions for the chi-squared test for independence */
proc sort data = cognition;
by descending ε4_carrier;
run;
/* Create a format for ε4_carrier and AB_positive to make the table understandable */
proc format;
value e4_carrier 1 = "Yes"
					 0 = "No";
value AB_positive 1 = "AB positive"
				 0 = "AB negative";
run;
proc freq data = cognition order=data;
title "Testing independence between APOE-ε4 carrier status and Aβ positivity status";
tables ε4_carrier * AB_positive / chisq expected fisher;
format ε4_carrier e4_carrier. AB_positive AB_positive.;
run;


/* Since at least 1/5 of our cells have expected cells less than 5, 
we should use Fisher's Exact Test to test for Independence. */
/* The Fisher's Exact Test has a p-value = 0.0108. This p-value is less than
alpha = 0.05, so we reject the null hypothesis.
We can conlude that there is sufficient evidence to conclude that APOE-ε4 carrier statuses 
are associated with Aβ positivity at a significance level of 5%. */


/* Next, we would perform a test for independence between sex and Aβ positivity */
/*H0: Sex is not associated with Aβ positivity
H1: Sex is associated with Aβ positivity. */
proc format; /* Create a format for Sex */

	value sex 
		1 = "Male"
		2 = "Female";
run;

proc freq data = cognition order=data;
title "Testing independence between sex and Aβ positivity status";
tables sex * AB_positive / chisq expected;
format sex sex. AB_positive AB_positive.;
run;
/* No more than 1/5 of our cells have expected cells <5, so we can perform a chi-squared test for independence. */
/* The chi-squared test yields a test statistic of 5.1983 and a p-value = 0.0226. 
Since our p-value is less than our set alpha = 0.05, we reject the null hypothesis. 
We have sufficient evidence to conclude that sex is
associated with Aβ positivity. */


/* Question 3: Are there differences in physical neurodegeneration (as outlined above) between APOE-ε4 carrier
statuses or sexes? */
/* Neurodegeneration is measured via MRI as cortical thickness (mm) in signature regions of the brain related to
Alzheimer’s disease. Calculate Cortical thickness in Alzheimer's disease signature regions */
data cognition;
set cognition;
	infpar_thickness = mean(infpar_lh, infpar_rh);
	inftemp_thickness = mean(inftemp_lh, inftemp_rh);
	precuneus_thickness = mean(precuneus_lh, precuneus_rh);
	supfront_thickness = mean(supfront_lh, supfront_rh);
	suppar_thickness = mean(suppar_lh, suppar_rh);
	supramarginal_thickness = mean(supramarginal_lh, supramarginal_rh);
	temporalpole_thickness = mean(temporalpole_lh, temporalpole_rh);
	
	entorhinal_thickness = mean(entorhinal_lh, entorhinal_rh); /* Used to calculate rostra_medial */
	parahippo_thickness = mean(parahippo_lh, parahippo_rh); /* Used to calculate rostra_medial */
	rostra_medial = mean(entorhinal_thickness, parahippo_thickness); /*Calculate the Rostral medial temporal cortical thickness */
	
	parsopercularis_thickness = mean(parsopercularis_lh, parsopercularis_rh); /* Used to calculate inferior frontal  */
	parsorbitalis_thickness = mean(parsorbitalis_lh, parsorbitalis_rh); /* Used to calculate inferious frontal  */
	parstriangularis_thickness = mean(parstriangularis_lh, parstriangularis_rh); /*Used to calculate inferior frontal */

/* Calculate mean inferior frontal cortical thickness */
	inffront_thickness = mean(parsopercularis_thickness, parsorbitalis_thickness, 
	parstriangularis_thickness);

/* Calculate the Cortical thickness in Alzheimer's disease signature regions */
	cortical_thickness = mean(infpar_thickness, inftemp_thickness, precuneus_thickness, 
	supfront_thickness, suppar_thickness, supramarginal_thickness, 
	temporalpole_thickness, rostra_medial, inffront_thickness);
run;

/* Check distribution for Cortical thickness in Alzheimer's disease signature regions */
proc sgplot data = cognition;
title "Distribution for Cortical thickness in Alzheimer's disease signature regions";
histogram cortical_thickness / scale=count;
density cortical_thickness;
xaxis label="Cortical thickness in Alzheimer's disease signature regions";
run;
/* The distribution for cortical thickness in Alzheimer's disease signature regions is approximately normal, so
cortical thickness will be kept as a continuous variable */


/* To determine if there are differences in physical neurodegeneration between APOE-ε4 carrier statuses, we can perform
an independent two sample t-test. H0: mu1(APOE-ε4 carrier) = mu2(non APOE-ε4 carrier). H1: mu1(APOE-ε4 carrier) =/= mu2(non APOE-ε4 carrier). */
proc ttest data = cognition;
title "T-test procedure observing if there are differences 
in physical neurodegeneration between APOE-ε4 carrier statuses";
class ε4_carrier;
var cortical_thickness;
format ε4_carrier e4_carrier.;
run;

/* The F-test of equal variances yields a p-value of 0.5260. This p-value is larger than our set alpha (Type 1 error) = 0.05, so we
fail to reject the null hypothesis of equal variance. We have insufficient data to conclude that the two samples are heteroscedastic
at a 5% significance level. Thus, we use the pooled method t-test for our hypothesis testing. */
/* The pooled t-test yields a test statistic of -0.04 and a p-value =0.9703. The p-value is larger than our set alpha = 0.05, so we fail
to reject the null hypothesis. We have insufficient evidence to conclude that there are differences in physical neurodegeneration between
APOE-ε4 carrier statuses. */
/* To determine if there are differences in physical neurodegeneration between sexes, we can perform
an independent two sample t-test. H0: mu1(Male) = mu2(Female). H1: mu1(Male) =/= mu2(Female). */

proc ttest data = cognition;
title "T-test procedure observing if there are differences 
in physical neurodegeneration between sexes";
class sex;
var cortical_thickness;
format sex sex.;
run;
/* The F-test of equal variances yields a p-value of 0.1223. This p-value is larger than our set alpha (Type 1 error) = 0.05, so we
fail to reject the null hypothesis of equal variance. We have insufficient data to conclude that the two samples are heteroscedastic
at a 5% significance level. Thus, we use the pooled method t-test for our hypothesis testing. */
/* The pooled t-test yields a test statistic of -3.40 and a p-value = 0.0008. The p-value is less than our set alpha = 0.05, so we
reject the null hypothesis. We have sufficient evidence to conclude that there are differences in physical neurodegeneration between sexes at a 5% significance level. */


/* Question 4: What is the distribution of APOE-ε4 between sexes? 
Do genetic risk factors for your outcomes behave differently between sexes? */

/* Checking the distribution of APOE-ε4 between the sexes */
proc freq data=cognition order=data;
table ε4_carrier*sex / chisq;
format sex sex. ε4_carrier e4_carrier.;
run;

/* Among the males in the dataset, 22.67 percent of them are APOE-ε4 carriers while
30.37 percent of females are also carriers. To test if genetic risk factors for if 
our outcomes behave differently between sexes, we must look at the interaction between
APOE-ε4 and sex. We can do this by building a logistic regression model. */

/* Building the logistic regression model */
proc logistic data=cognition;
title "Logistic model observing interaction between sex and  ε4 carrier status";
class sex(ref="Male") / param=ref;
class ε4_carrier(ref="No") / param=ref;
model AB_positive(event='AB positive') = sex ε4_carrier ε4_carrier*sex; 
format sex sex. ε4_carrier e4_carrier. AB_positive AB_positive.;
run;

/* According to the joint tests table, the p-value of the 
ε4_carrier*sex interaction term is 0.6723. At an alpha level of 0.05, we would fail to
reject the null hypothesis that there is no association between ε4_carrier and sex and 
conclude that there is insufficient evidence of a significant relationship between the 
two variables at a 5% level of significance. */

/* Question 5: Are your effects consistent after adjusting for potential confounders? 
Thoughtfully consider which covariates to include in your model. In past studies, evidence
has shown that age, sex, depression, low physical activity, and adiposity (high waist
circumference) may be risk factors for vascular dementia. */

/* Creating adiposity categorical model */
data cognition;
set cognition;

/* Defining adiposity categorical variable */
if (waist >= 102 and sex=1) or (waist >= 89 and sex=2) then adiposity = 1; 
else adiposity = 0;
run;

/* Creating dummy variables required */
data cognition;
set cognition;

/* IPAQ dummy variables (IPAQcat=1 is the reference) */
IPAQ_moderate = 0;
if IPAQcat = 2 then IPAQ_moderate = 1;
IPAQ_high = 0;
if IPAQcat = 3 then IPAQ_high = 1;

/* PHQ9 dummy variables (PHQ9=0 is the reference) */
phq9_low = 0;
if phq9 = 1 then phq9_low=1;
phq9_mild = 0;
if phq9 = 2 then phq9_mild = 1;
phq9_moderate = 0;
if phq9 = 3 then phq9_moderate = 1;
phq9_severe = 0;
if phq9 = 4 then phq9_severe = 1;
run;

/* Formatting the phq9, IPAQcat, and adiposity variables */
proc format;
	value phq
		0 = 'No symptoms'
		1 = 'Minimal symptoms'
		2 = 'Mild symptoms'
		3 = 'Moderate symptoms'
		4 = 'Severe symptoms';
		
	value IPAQcat
		1 = 'Low'
		2 = 'Moderate'
		3 = 'High';
		
	value adiposity
		1='Yes'
		0='No';
run;

/* Based on this question, a stepwise function will need to be used (one of these) */
proc logistic data=cognition;
class sex(ref='Male') ε4_carrier (ref='No') adiposity (ref='No') / param=ref;
model AB_positive =  ε4_carrier age sex adiposity phq9_low
phq9_mild phq9_moderate phq9_severe IPAQ_moderate IPAQ_high 
/ selection=stepwise slstay=0.15 slentry=0.15;
format adiposity adiposity. sex sex. ε4_carrier e4_carrier.;
run;

/* After running a logistic regression model with slstay and slentry values of 0.15
and a stepwise selection process, we found that ε4_carrier, sex, and being in the 
high IPAQ category to be significant predictors of AB positivity status.*/

/* Calculating crude and adjusting models to assess confounding */
/* Crude model (contans only ε4_carrier) */
proc logistic data=cognition;
title "Crude model";
class ε4_carrier (ref='No') / param=ref;
model AB_positive =  ε4_carrier / cl;
format ε4_carrier e4_carrier.;
run;

/* Adjusted model (contans ε4_carrier, sex and IPAQ_high)*/
proc logistic data=cognition;
title "Adjusted model";
class sex(ref='Male') ε4_carrier (ref='No') / param=ref;
model AB_positive =  ε4_carrier sex IPAQ_high / cl;
format sex sex. ε4_carrier e4_carrier.;
run;

/* % change calculations: (-0.9580 - (-0.8746)) / -0.9580 = 0.0871 */
/* When comparing the AB_positive slopes between the crude and adjusted models,
we saw an 8.71 percent change when ε4_carrier sex IPAQ_high were modeled (adjusted)
compared to when it was only ε4_carrier being modeled (crude). Based on the 10 percent
rule of confounding, we can conclude that sex IPAQ_high are not confounders of
ε4 carrier status and AB_positive status. */

/* Calculating summary tables of data (to reference when I write manuscript) */

/* Calculating how many of each AB_positive status in dataset */
proc freq data=cognition;
title "Table of Frequencies by AB_positive status";
table AB_positive sex;
format AB_positive AB_positive. sex sex.;
run;

/* Important note:
Table below has 265 observations rather than 266. A female non-AB positive observation
was removed due to missing data. Manuscript will state n=265. */
proc tabulate data=cognition;
title "Table of Summary Statistics";
var age waist education hip;
class ε4_carrier phq9 ipaqcat sex AB_positive;
tables (age waist hip education)*(mean stddev p25 p75)
(sex ε4_carrier phq9 ipaqcat)*(n colpctn), AB_positive / misstext='0';
format AB_positive AB_positive. ε4_carrier e4_carrier.
ipaqcat ipaqcat. sex sex. phq9 phq.;
run;

ods rtf close;