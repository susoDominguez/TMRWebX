---
address: 'King''s College London'
author:
- Jesus Dominguez
title: 'ROAD2H Interpretable-clinical guideline authoring server (eXtension) adapting the TMR model: TMRwebX'
---

Introduction
============

This document summarises how to interact with the ROAD2H guideline interaction
service, TMRwebX, which packages TMR [@Zamborlini2016] as a web service,
allowing for the representation of clinical guidelines as a set of recommendations in an interoperable manner. 
Additionally, it provides as set of logic rules to identify potential interactions -alternative, repetition, contradiction, repairable- among these guidelines.

Scenario
========

![Guideline set *HT* (hypertension), consisting of *Diueretic2* and
*Diuretic*.[]{label="HT"}](CIG-HT.png){#HT width="0.8\linewidth"}

Figure [1](#HT){reference-type="ref" reference="HT"} shows two
hypertension recommendations. This is also a type of interaction known as a
*repairable transition*, because the actions undertaken by one recommendation 
(in this case I recommendation to avoid a care action, yet it may be required to adminsiter it regardless)
can be reversed by performing the actions associated with the other. In
text form, these guidelines might read '*To reduce a patient's blood
pressure, administer Thiazide. Similarly, to avoid exacerbating a
patient's blood pressure, avoid the administration of Ibuprofen.*'

In the example detailed in this document, our aim is to represent these
recommendations using the semantic format used by TMR, and to then use a
computational implementation of TMR to identify the repairable
transition shown, features which are both offered by TMRwebX. This then
serves as an example for how to use TMRwebX to create new guideline sets,
and identify interactions between the constituent guidelines.

In Figure [1](#HT){reference-type="ref" reference="HT"}, the second
recommendation, Reduce Blood Pressure, will be referred to using the ID
*Diuretic*; the first, Avoid High Blood Pressure, using the ID
*Diuretic2*; and the recommendations as a guideline using the ID *HT*.

Implementation
==============

TMRwebX is a RESTful web service, which accepts HTTP POST requests.
Therefore, to construct our semantic representation, and to then
interrogate this implementation, requests will be issued to this
service, using the *CURL* command[^1]. 
Examples are given throughout the document, and should always be accompanied by the following `header`
information:

      --header `Content-Type: application/x-www-form-urlencoded'
      
Also notice the URL is localhost as we have deployed the app locally for providing this examples.


Example representation process {#example}
==============================

To represent the set of guidelines shown in Figure
[1](#HT){reference-type="ref" reference="HT"} and have TMRwebX
successfully leverage TMR to identify the interactions between them, we
first need to define the world knowledge that supports their definition
and allows TMR to operate: the existence of drugs; the concept of a
patient having a certain medical state, such as a blood pressure level;
the concept of altering a person's medical state, such as their blood
pressure; and the effects of taking a drug.

Care Actions
-----

To define a drug, we might start by representing a general category of
drugs. Here, we create a dummy category *Thiazide* for *Diuretic*:


      curl --location --request POST 'http://localhost:8888/tmrweb/careAction/drug/category/add' \
      --data-urlencode 'drugCat_id=Thiazide' \
      --data-urlencode 'drugCat_label=Thiazide' \
      --data-urlencode 'action_label=Thiazide' \


When we define a category, we can also specify which drugs are in this
category (in this case only Thiazide), and general properties of the
drugs in this category, but this falls outside the scope of this
example.

      --data-urlencode 'subsumed_drug_ids=Aspirin%2C%20Ibuprofren' \
      --data-urlencode 'grouping_criteria_ids=SitNonSteroidalDrug%2C%20TrAntinflammatory' \
     
Moreover, we could add clinical codes for ICD10, UMLS or SNOMED CT code schemes 
(below, and for the rest of the example) we  use  dummy code 000 for all three cases)

      --data-urlencode 'snomedCodes=000' \
      --data-urlencode 'icd10Codes=000' \
      --data-urlencode 'umlsCodes=000'

Given this category, we can now define the actual *Diuretic* drug, and
state that it is part of this category. We can also state an individual
subsuming relationship to another drug type (in this case not one we have priorly
represented):

    curl --location --request POST 'http://localhost:8888/tmrweb/careAction/drug/individual/add' \
      --data-urlencode 'drug_id=Thiazide' \
      --data-urlencode 'drug_label=Thiazide' \
      --data-urlencode 'subsumed_drug_id=Bendroflumethiazide' \
      --data-urlencode 'action_label=Thiazide' \


We follow a similar approach for *Diuretic2*, which pertains to the drug
Ibuprofen and in this instance there is no subsuming drug type relation:

    curl --location --request POST 'http://localhost:8888/tmrweb/careAction/drug/individual/add' \
      --data-urlencode 'drug_id=Ibuprofen' \
      --data-urlencode 'drug_label=Ibuprofen' \
      --data-urlencode 'action_label=Ibuprofen' \

We have now defined the drugs shown in Figure
[1](#HT){reference-type="ref" reference="HT"}.

Aditionally, one can add non-drug types (non-drug care actions) like, pulmonary rehabilitation (or a CT scan, etc).

      curl --location --request POST 'http://localhost:8888/tmrweb/careAction/nondrug/individual/add' \
      --data-urlencode 'nonDrug_id=LngRehab' \
      --data-urlencode 'nonDrug_label=LngRehab' \
      --data-urlencode 'nonDrugAct_label=administer pulmonary rehabilitation' \
      
To check available drug types and categories

      curl --location --request POST 'http://localhost:8888/tmrweb/careActions/drugs/get' \
      
Similarly, to check available non-drug types

      http://localhost:8888/tmrweb/careActions/nonDrugs/get
      
To delete a drug type or drug category, we must insert all the relevant data that must be discarded.
For instance, the following code deletes the ALL knowledge we have on the drug type Thiazide

      curl --location --request POST 'http://localhost:8888/tmrweb/careAction/individual/delete' \
      --data-urlencode 'drug_id=Thiazide' \
      --data-urlencode 'drug_label=Thiazide' \
      --data-urlencode 'subsumed_drug_id=Aspirin%2C%20Ibuprofren' \
      --data-urlencode 'grouping_criteria_ids=SitNonSteroidalDrug%2C%20TrAntinflammatory' \
      --data-urlencode 'action_label=Thiazide' \
      --data-urlencode 'icd10Codes=000' \
      --data-urlencode 'snomedCodes=000' \
      --data-urlencode 'atcCodes=000' \
      --data-urlencode 'umlsCodes=000'

Situation
---------

We next define the situations a patient might find themselves in, in
respect of their vitals, which in the case of *HT* is varying levels of
blood pressure (Figure [1](#HT){reference-type="ref" reference="HT"}).
Specifically, we define the state in which a patient has normal blood
pressure, and the state in which a patient has high blood pressure. 
We can also add additional information such as the clinical codes that
might be used to reference such states in an EHR:

    curl --location --request POST 'http://localhost:8888/tmrweb/transition/situation/add' \
      --data-urlencode 'situation_id=NormalBP' \
      --data-urlencode 'stateOfproperty=normal' \
      --data-urlencode 'situation_label=Blood%20pressure%20is%20normal' \
      --data-urlencode 'umlsCodes=null' \
      --data-urlencode 'snomedCodes=null' \
      --data-urlencode 'icd10Codes=null'

    curl --location --request POST 'http://localhost:8888/tmrweb/transition/situation/add' \
      --data-urlencode 'situation_id=HighBP' \
      --data-urlencode 'stateOfproperty=high' \
      --data-urlencode 'situation_label=Blood%20pressure%20is%20high' \
      --data-urlencode 'umlsCodes=C0020538%2C%20C3843080' \
      
Similarly as with drug types, situations can also be deleted via an endpoint ending on `/tmrweb/transition/situation/add`

One can query the database to get all available knowledge on a situation, by using its given situation id or the URI created when adding it to the database

      curl --location --request POST 'http://localhost:8888/tmrweb/transition/situation/all/get' \
      --data-urlencode 'situation_uri=http://anonymous.org/data/SitNormalBP' \
      
      curl --location --request POST 'http://localhost:8888/tmrweb/transition/situation/all/get' \
      --data-urlencode 'situation_id=NormalBP'

Measured clinical property
--------

Next, we identify the clinical property being measured on both situations, that is, the blood pressure

      curl --location --request POST 'http://localhost:8888/tmrweb/transition/property/add' \
      --data-urlencode 'property_id=BP' \
      --data-urlencode 'property_label=blood pressure'
    
Again, data on a property can be deleted/retrieved using the following endpoints 
`/tmrweb/transition/property/delete` and `/tmrweb/transition/property/all/get`
using parameters similarly to previous cases.


Transition
----------

We next model transitions between the situations specified previously,
specifically moving between different blood pressure levels, as shown in
Figure [1](#HT){reference-type="ref" reference="HT"}.

    curl --location --request POST 'http://localhost:8888/tmrweb/transition/add' \
      --data-urlencode 'transition_id=IncreaseBP' \
      --data-urlencode 'pre_situation_id=NormalBP' \
      --data-urlencode 'post_situation_id=HighBP' \
      --data-urlencode 'affected_property_id=BP' \
      --data-urlencode 'derivative=increase'
    
    curl --location --request POST 'http://localhost:8888/tmrweb/transition/add' \
      --data-urlencode 'transition_id=DecreaseBP' \
      --data-urlencode 'pre_situation_id=HighBP' \
      --data-urlencode 'post_situation_id=NormalBP' \
      --data-urlencode 'affected_property_id=BP' \
      --data-urlencode 'derivative=decrease'
      
Data on transitions can be deleted/retrieve using the following endpoints
`http://localhost:8888/tmrweb/transition/delete` and
`http://localhost:8888/tmrweb/transition/all/get` using parameters similarly to previous cases.

Causation Beliefs
-------

Finally, we combine our drug information (Section
[4.1](#drugs){reference-type="ref" reference="drugs"}) and transition
information (Section [4.3](#transition){reference-type="ref"
reference="transition"}) to construct beliefs about the potential effects of
administering a drug, for *Diuretic* and *Diuretic2* (Figure
[1](#HT){reference-type="ref" reference="HT"}) (or a non-drug care action):

    curl --location --request POST 'http://localhost:8888/tmrweb/belief/add' \
      --data-urlencode 'belief_id=ThiazideBP' \
      --data-urlencode 'careAct_cause_id=Thiazide' \
      --data-urlencode 'transition_effect_id=DecreaseBP' \
      --data-urlencode 'strength=L1' \
      --data-urlencode 'author=JDA'

    curl --location --request POST 'http://localhost:8888/tmrweb/belief/add' \
      --data-urlencode 'belief_id=IbuprofenBP' \
      --data-urlencode 'careAct_cause_id=Ibuprofen' \
      --data-urlencode 'transition_effect_id=IncreaseBP' \
      --data-urlencode 'strength=L1' \
      --data-urlencode 'author=JDA'

We can add additional information, such as a code to indicate the
strength of the belief, and how often this belief applies to the
referenced transition.

The set of all causation beliefs URIs can be retrieved using

      http://localhost:8888/tmrweb/beliefs/get
     
As depicted in cases above above, beliefs can also be deleted/retrieved.

Guidelines
----------

With all of our background information specified, we can now construct
our actual guidelines, combined in a guideline group, which brings all
of this information together.

First, we supply some details for our guideline group[^2]:

    curl --location --request POST 'http://localhost:8888/tmrweb/guideline/create' \
      --data-urlencode 'cig_id=HT' \
      --data-urlencode 'description=CIG%20for%20hypertension' \
      --data-urlencode 'IsPersistent=true'

Then, we specify information for *Diuretic* in the form of a recommendation,
including the remaining information from Figure [1](#HT){reference-type="ref" reference="HT"},
such as the guideline name, and the nature of the associated recommendation (should or should not be recommended):

    curl --location --request POST 'http://localhost:8888/tmrweb/guideline/rec/add' \
      --data-urlencode 'cig_id=HT' \
      --data-urlencode 'rec_id=Diuretic' \
      --data-urlencode 'careAction_id=Thiazide' \
      --data-urlencode 'belief_id=ThiazideBP' \
      --data-urlencode 'label=Reduce%20blood%20pressure' \
      --data-urlencode 'isRecommended=true' \
      --data-urlencode 'author=Jesus' \
      --data-urlencode 'contribution=positive' \
      --data-urlencode 'source=https://hypertension.org/Pocket-Guide.pdf'
  

*Diuretic2* is also defined accordingly:

    curl --location --request POST 'http://localhost:8888/tmrweb/guideline/rec/add' \
      --data-urlencode 'cig_id=HT' \
      --data-urlencode 'rec_id=Diuretic2' \
      --data-urlencode 'careAction_id=Ibuprofen' \
      --data-urlencode 'belief_id=IbuprofenBP' \
      --data-urlencode 'label=Avoid%20high%20blood%20pressure' \
      --data-urlencode 'isRecommended=false' \
      --data-urlencode 'author=Jesus' \
      --data-urlencode 'contribution=negative' \
      --data-urlencode 'source=https://hypertension.org/Pocket-Guide.pdf'
      
A dataset (guideline) can be deleted providing its identifier:

      curl --location --request POST 'http://localhost:8888/tmrweb/guideline/delete' \
            --data-urlencode 'cig_id=CIG-HT'

The set of recommendation URIs from a dataset can be retrieved as follows

      curl --location --request POST 'http://localhost:8888/tmrweb/guidelines/rec/get' \
            --data-urlencode 'cig_id=CIG-HT'
            
Retrieve a particular recommendation (in JSON format) from a dataset by means of its URI

      curl --location --request POST 'http://localhost:8888/tmrweb/guideline/rec/all/get/' \
            --data-urlencode 'rec_URI=http://anonymous.org/data/RecHT-Diuretic' \
            --data-urlencode 'cig_id=HT'

Sub-guidelines
--------------

Applying all recommendations at once from a guideline may not be benefitial. 
One could group relevant parts of a guideline togehter as a whole, to be retrieved as part of some particular clinical scenario.
We call it a sub-guideline and it is comprised of one or more recommendations from the same dataset.

To create a sub-guideline, identify the dataset and the recommendations ids to be grouped togheter under the sub-guideline.
In the example below, we identify both recommendations as part of the same sub-guideline we call `BP`.

      curl --location --request POST 'http://localhost:8888/tmrweb/guideline/subguideline/add' \
            --data-urlencode 'guideline_id=HT' \
            --data-urlencode 'subGuideline_id=BP' \
            --data-urlencode 'recs_ids=Diuretic,Diuretic2' \
            --data-urlencode 'description=recommendations affecting blood pressure'

Similar to previous examples, there is code to delete a sub-guideline from some specific dataset.

One can also copy knowledge from one or more subguidelines in a dataset and add it to another existing dataset

      curl --location --request POST 'http://localhost:8888/tmrweb/guidelines/add' \
            --data-urlencode 'cig_from=CIG-HT' \
            --data-urlencode 'cig_to=CIG-OTHER' \
            --data-urlencode 'subguidelines=subCIG-BP'
            
Example interrogation process (single interaction)
==================================================

Now that we have our guideline set represented, we can interrogate it in
order to find any interactions in the following way:

    curl --location --request POST 'http://localhost:8888/tmrweb/guidelines/interactions' \
      --data-urlencode 'cig_id=HT'

This gives us the following textual response (we have currently extended the outcome format to deliver it using a JSON structure):

``` {frame="none"}
[interaction(http://anonymous.org/data/ReparableTransitionRecHT-Diuretic2RecHT-Diuretic,Reparable Transition,[http://anonymous.org/data/RecHT-Diuretic,http://anonymous.org/data/RecHT-Diuretic2],[])]
```

The first element of this interaction object is a unique name for the
identified interaction, the next the type of interaction, the third the
fully qualified IDs, and finally any additional external information. We
can see that, as expected, the TMR reasoning engine within TMRweb has
identified the interaction shown within Figure
[1](#HT){reference-type="ref" reference="HT"}.

Using this information, we can learn more about the guidelines involved
in the interaction, and thus re-acquire the information shown in Figure
[1](#HT){reference-type="ref" reference="HT"}, from TMRweb. For example,
we can first ask which drugs each guideline in the interaction relates
to:

    curl --request POST \
      --url https://localhost:8888/tmrweb/guidelines/drug \
      --data `guideline_id=http%3A%2F%2Fanonymous.org%2Fdata%2FRecHT-Diuretic&guideline_group_id=HT'

``` {frame="none"}
http://anonymous.org/data/DrugCatThiazide
```

    curl --request POST \
      --url https://consult.hscr.kcl.ac.uk/tmrweb/guidelines/drug \
      --data `guideline_id=http%3A%2F%2Fanonymous.org%2Fdata%2FRecHT-Diuretic2&guideline_group_id=HT'

``` {frame="none"}
http://anonymous.org/data/DrugTIbuprofen
```

We can then ask what the effects of these drugs are, and thus learn why
they are considered alternative actions:

    curl --request POST \
      --url https://localhost:8888/tmrweb/drugs/effects \
      --data drug_full_id=http%3A%2F%2Fanonymous.org%2Fdata%2FDrugCatThiazide

``` {frame="none"}
http://anonymous.org/data/ActAdministerThiazide causes http://anonymous.org/data/TrDecreaseBP
```

    curl --request POST \
      --url https://localhost:8888/tmrweb/drugs/effects \
      --data drug_full_id=http%3A%2F%2Fanonymous.org%2Fdata%2FDrugTIbuprofen

``` {frame="none"}
http://anonymous.org/data/ActAdministerIbuprofen causes http://anonymous.org/data/TrIncreaseBP
```

[^1]: *Postman* (<https://www.getpostman.com/>), neatly wraps CURL
    commands.

[^2]: For now, we pre-configure TMRweb with each guideline group ID,
    outside of the REST interface.
