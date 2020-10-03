'use strict';

function Card( uuid, patient, resourcesList) {
    this.uuid = uuid ;
    this.patient = patient ;
    this.patientFhirId = 'Patient/' + patient;
}
const cdsCard = {
	"cards": [
		{
			"summary": "mitigated COPD proposed care plan",
			"indicator": "info",
			"source": {
				"label": "GOLD 2017 COPD guideline"
			},
			"suggestions": [
				{
					"label": "COPD care plan decision support",
					"uuid": "CIG-2803202017350345",
					"actions": [
						{
							"type": "update",
							"description": "Update COPD care plan",
							"resource": {
								"resourceType": "Bundle",
								"id": "COPDbundle",
								"type": "collection",
								"entry": []
							}
						}
					]
				}
			],
			"selectionBehaviour": "at-most-one"
		}
	]
};

//patient entry on cds cards (action array)
const patientEntry = {
    fullUrl: "http://acme.com/Patient/pat1", //TODO: add pat id
    resource: {
        resourceType: "Patient",
        id: "pat1", //TODO: add patient id
        active: true,
        birthDate: "1974-12-25" //TODO: add birthdate
    }
};

//class to represent Medication FHIR resources
class FhirMedication {

    /**
     * 
     * @param {string} url 
     * @param {object} careActionTypeObject 
     */
    constructor(url, careActionTypeObject) {

        this._id = String(careActionTypeObject.code);
        this._codingSystem = String(careActionTypeObject.id);
        this._codingCode = String(careActionTypeObject.drugLabel);
        this._codingDisplay = String(careActionTypeObject.display);
        this._fullUrl = url;
        this._codeObject = {
            coding:
                [
                    {
                        system: this._codingSystem,
                        code: this._codingCode,
                        display: this._codingDisplay
                    }
                ]
        };

    }

    /**
     * Transforms the instance into a JS object
     * 
     * @returns {Object}
     */
    toJSON() {

        return {
            fullUrl: this._fullUrl,
            resource:
            {
                resourceType: "Medication",
                id: this._id,
                code: this._codeObject
            }
        };
    }
}

//class to represent Condition FHIR resources
class FhirCondition {

    /**
     * @param {string} fullUrl
     * @param {object} preSit 
     * @param {string} patient 
     */
    constructor(fullUrl, preSit, patient) {
        this._id = String(preSit.value.code);
        this._codingSystem = String(preSit.id);
        this._codingDisplay = String(preSit.value.display);
        this._fullUrl = fullUrl;
        this._patient = 'Patient/' + (patient) ? String(patient) : 'dummy';
        this._codeObject = {
            coding:
                [
                    {
                        system: this._codingSystem,
                        code: this.id,
                        display: this._codingDisplay
                    }
                ]
        };
    }

    /**
     * Transforms the instance into a JS object
     * 
     * @returns {Object}
     */
    toJSON() {

        return {
            fullUrl: this._fullUrl,
            resource:
            {
                resourceType: 'Condition',
                id: this._id,
                code: this._codeObject,
                subject: {
                    reference: this._patient
                }
            }
        };
    }
}

//class to represent ForecastEffect FHIR resources
class FhirForecastEffect {

    constructor(tmrRecId, fullUrl, fhirID, situations, beliefInstance, beliefIndex, patient) {

        //constants of the class forecastEffect
        let main = 'main-effect';
        let side = 'side-effect';
        let positive = 'adverse-event';
        let negative = 'therapeutic-event';

        this._medicationRequestList = [];
        this.addMedicationRequestUrl(String(tmrRecId));

        const situations = this.getSituations(beliefInstance);

        //temp let containing identifier of this ForecastEffect instance
        this._id = fhirID;
        this._fullUrl = fullUrl;
        this._effectType = (beliefIndex === 0) ? main : side;
        this._eventType = (String(beliefInstance.contribution) == 'positive') ? positive : negative;
        this._probability = String(beliefInstance.probability);
        this._evidenceLevel = String(beliefInstance.evidence);
        this._expectedCodingSystem = String(situations.postSituation.id);
        this._expectedCodingCode = String(situations.postSituation.value.code);
        this._expectedCodingDisplay = String(situations.postSituation.value.display);
        this._propCodingSystem = String(beliefInstance.transition.property.id);
        this._propCodingCode = String(beliefInstance.transition.property.code);
        this._propCodingDisplay = String(beliefInstance.transition.property.display);
        this._degree = String(beliefInstance.transition.effect);
        this._patient = 'Patient/' + (patient)?  patient : 'dummy';
        //convert the recomendation uri into a medication request
        
        this._condition = 'Condition/' + situations.preSituation.value.code;
    }
  
    addMedicationRequestUrl(recId) {
        let medReq = 'MedicationRequest/' + recId.slice(26);
        this._medicationRequestList.push({ reference: medReq });
    }

    toJSON() {
        return {
            fullUrl: this._fullUrl,
            resource: {
                resourceType: "ForecastEffect",
                id: this._id,
                typeOfEffect: this._effectType,
                typeOfEvent: this._eventType,
                subject: {
                    reference: this._patient
                },
                appliesTo: {
                    careActionInstance: this._medicationRequestList,
                    conditionAddressed: {
                        reference: this._condition
                    }
                },
                expectedOutcomeCode: {
                    coding: [
                        {
                            system: this._expectedCodingSystem,
                            code: this._expectedCodingCode,
                            display: this._expectedCodingDisplay
                        }
                    ]
                },
                targetMeasurement: {
                    measuredProperty: {
                        coding: [
                            {
                                system: this._propCodingSystem,
                                code: this._propCodingCode,
                                display: this._propCodingDisplay
                            }
                        ]
                    },
                    degreeOfChange: this._degree
                },
                probability: this._probability,
                evidence: this._evidenceLevel
            }
        };
    }

}

class FhirMedicationRequest {


    /**
     * @param {string} fullUrl FHIR entry element unique ULR
     * @param {string} id FHIR resource identifier
     * @param {object} recObject TMR recomendation object
     * @param {object} interactions TMR interactions object
     * @param {string} patient patient id
     */
    constructor(fullUrl, id, recObject, interactions, patient) {

        this._conditionList = [];
        this._forecastEffectList = [];
        this._detectedIssueList = [];
        this._fullUrl = fullUrl;
        this._id = id;
        this._cigUri = String(recObject.derivedFrom);
        this._patient = 'Patient/' + (patient !== undefined) ? patient : 'dummy';
        this._medication = 'Medication/' + String(recObject.careActionType.code);
        this._doNotRecommend = String(recObject.suggestion) === 'nonrecomend';
        //create list of Conditions and ForecastEffects -both have the same number of resources
        this.createResourcesList(recObject.causationBeliefs);
        this.addDetectedIssues(interactions);
    }

    getSituations(situationTypeList) {

        let preSit = JSON.parse(situationTypeList[0]);
        let postSit = JSON.parse(situationTypeList[1]);

        //check it is not the other way around
        if (String(postSit.type) === 'hasTransformableSituation') {
            //copy not shared
            let temp = JSON.parse(JSON.stringify(postSit));
            postSit = preSit;
            preSit = temp;
        }

        return { preSituation: preSit, postSituation: postSit };
    }

    //TODO: add method to create detected issue list
    addDetectedIssues(interactionList){
        for (let index = 0; index < interactionList.length; index++) {
            const interaction = interactionList[index];
            if(interaction){
                const interType = interaction.type;
                const norms = interaction.interactionNorms;
                const prefix = 'DetectedIssue/';
                let recomId;
                let recomType;
                if(Array.isArray(norms)){

                    for (let i = 0, len = norms.length; i < len; i++) {

                         recomId = String(entry.recId) ;
                         recomType = String(entry.type) ;   

                        //reconstruct TMR URI for recommendation
                        const id = ('http://anonymous.org/data/' + this.id) ;

                        if(id === recomId){

                            let refId = prefix + interType + i ;
                            this._detectedIssueList.push({
                                reference: refId
                            });

                            //break the loop
                            break;
                        }
                    }
                }
                
            }
            
        }

    }
    

    /**
     * 
     * @param {Array<object>} causationBeliefList list of causation beliefs in a given TMR-based recomendation
     * 
     * @returns {object} an object of form {conditionList:: Array<object>, forecastEffectList:: Array<object>}
     */
    createResourcesList(causationBeliefList) {

        //add a reference to both lists above
        for (let beliefIndex = 0; beliefIndex < causationBeliefList.length; beliefIndex++) {

             //retrieve causation belief
             let cb = causationBeliefList[beliefIndex];

            //extract pre and post situation objects
            const sitObject = this.getSituations(cb.situationTypes);
            //retrieve condition id
            let preSituation = sitObject.preSituation;;
            const condition = 'Condition/' + preSituation;
            //retrieve forecastEffectId
            let postSituation = sitObject.postSituation;
            let isMainEffect = beliefIndex === 0;
            let forecastEffectId = preSituation + '2' + postSituation + ( isMainEffect? 'M' : 'S') + cb.contribution.charAt(0);
            const forecastEffect = 'ForecastEffect/' + forecastEffectId;
            //add condition id if this is the main effect (side effects conditions can be fetched via forecast effects)
            if(isMainEffect) this._conditionList.push({ reference: condition });
            //add forecastEffect id
            this._forecastEffectList.push({ reference: forecastEffect });
        }
    }


    toJSON() {
        return {
            fullUrl: this._fullUrl,
            resource: {
                resourceType: "MedicationRequest",
                id: this._id,
                status: "active",
                intent: "plan",
                instantiatesUri: this._cigUri,
                doNotPerform: false,
                reasonReference: this._conditionList,
                "forecast-effects": this._forecastEffectList,
                medicationReference: {
                    reference: this._medication
                },
                subject: {
                    reference: this._patient
                },
                detectedIssue: this._detectedIssueList
            }
        };
    }
}

class FhirServiceRequest {

    /**
    * 
    * @param {object} recObject TMR recomendation object
    * @param {string} patient patient id
    */
    constructor(fullUrl, id, recObject, patient) {

        this._conditionList = [];
        this._forecastEffectList = [];

        const careActionTypeObject = recObject.careActionType;
        this._serviceSystem = String(careActionTypeObject.id);
        this._serviceCode = String(careActionTypeObject.drugLabel);
        this._serviceDisplay = String(careActionTypeObject.display);
        //this._medRequestUrlPrefix = 'http://anonymous.org/MedicationRequest/';
        this._fullUrl = fullUrl;
        this._id = id;
        this._cigUri = String(recObject.derivedFrom);
        this._patient = 'Patient/' + (patient) ? patient : 'dummy';
        this._medication = 'Medication/' + String(recObject.careActionType.code);
        this._doNotRecommend = String(recObject.suggestion) === 'nonrecomend';
        //create list of Conditions and ForecastEffects -both have the same number of resources
        this.createResourcesLists(recObject.causationBeliefs);
    }

    /**
     * 
     * @param {object} tmrRecObject TMR-based recomendation in JSON notation
     */
    showFullUrl(tmrRecObject) {
        return 'http://anonymous.org/MedicationRequest/' + String(tmrRecObject.id).slice(26);
    }

    /**
     * 
     */
    toJSON() {
        return {
            fullUrl: this._fullUrl,
            resource: {
                resourceType: "ServiceRequest",
                id: this._id,
                status: "active",
                intent: "plan",
                instantiatesUri: this._cigUri,
                doNotPerform: false,
                reasonReference: this._conditionList,
                "forecast-effects": this._forecastEffectList,
                code: {
                    coding: [
                        {
                            system: this._serviceSystem,
                            code: this._serviceCode,
                            display: this._serviceDisplay
                        }
                    ]
                },
                subject: {
                    reference: this._patient
                }
            }
        };
    }
}

class FhirDetectedIssue {

    _fullUrl;
    _issueSystem;
    _issueCode;
    _issueDisplay;
    _implicatedList = [];
    _mitigation1;


    toJSON() {
        return {
            fullUrl: this._fullUrl,
            resource: {
                resourceType: "DetectedIssue",
                id: this._id,
                status: "preliminary",
                code: {
                    coding: [
                        {
                            system: this._issueSystem,
                            code: this._issueCode,
                            display: this._issueDisplay
                        }
                    ]
                },
                implicated: this._implicatedList
            }
        };
    }
}

class FhirCarePlan {

    _fullUrl;
    _id;
    _title;
    _patient;
    _activityList;

    toJSON() {
        return {
            fullUrl: this._fullUrl,
            resource: {
                resourceType: "CarePlan",
                id: this._id,
                status: "active",
                intent: "plan",
                title: this._title
            },
            subject: {
                reference: this._patient
            },
            activity: this._activityList
        };
    }
}

/////////////////////// Classes holding resources

class MedicationResources {

    /**
     * 
     * @param {Map<string, FhirMedication>} map Create a Map of medication resources
     */
    constructor(map) {
        //Map containing medication resources
        this.map = map;
    }

    /**
     * 
     * @param {object} careAction an intance object of a TMR-based care action type
     */
    showFullUrl(careAction) {
        return 'http://anonymous.org/Medication/' + careAction.code;
    }

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return (tmrRec) => {
            //TODO: add error checker for objects that do not have careActionType properties
            let careAction = tmrRec["careActionType"];
            //if not undefined, act on it
            if (careAction) {
                //create Fhir URL
                const url = this.showFullUrl(careAction);
                //if it does find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirMedication(url, careAction));
                }
            }
        };
    }

    get resourcesList() {
        //map values to array
        return Array.from(this.map.values());
    }
}
class ConditionResources {

    /**
     * 
     * @param {Map<string, FhirCondition>} map Create a Map of condition resources
     */
    constructor(map) {
        //Map containing  resources
        this.map = map;
    }

    showFullUrl(preSitObject) {

        return 'http://anonymous.org/Condition/' + String(preSitObject.value.code);
    }

    getPreSitObject(situationTypeList){
        return (String(situationTypeList[0].type) === 'hasTransformableSituation') ? situationTypeList[0] : situationTypeList[1];
    }

    add() {
        //obtain argument from the calling function
        return (cb, patient) => {

            //if not undefined, act on it
            if (cb) {
                let preSit = this.getPreSitObject(cb.situationTypes);
                //create Fhir URL
                const url = this.showFullUrl(preSit);
                //if it does find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirCondition(url, preSit, patient));
                }
            }
        };
    }

    /**
     * create a JSON object containing all  resources.
     */
    get ResourceList() {
        //map values to array
        return Array.from(this.map.values());
    }
}
class FhirForecastEffectResources {

    /**
     * 
     * @param {Map<string, FhirForecastEffect>} map Create a Map of  resources
     */
    constructor(map) {
        //Map containing medication resources
        this.map = map;
    }

    getId(preSituation, postSituation, beliefIndex, contribution) {
        return String(preSituation) + '2' + String(postSituation) + ((beliefIndex === 0) ? 'M' : 'S') + contribution.charAt(0);
    }

    getSituations(situationTypeList) {

        let preSit = JSON.parse(situationTypeList[0]);
        let postSit = JSON.parse(situationTypeList[1]);

        //check it is not the other way around
        if (String(postSit.type) === 'hasTransformableSituation') {
            //copy not shared
            let temp = JSON.parse(JSON.stringify(postSit));
            postSit = preSit;
            preSit = temp;
        }

        return { preSituation: preSit, postSituation: postSit };
    }

     showFullUrl(fhirId) {
        return 'http://anonymous.org/ForecastEffect/' + fhirId;
    }

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return ({recId, beliefInstance, index, patient}) => {
            //if not of type undefined, act on it
            if (beliefInstance) {
                const sitObject = this.getSituations(beliefInstance.situationTypes);
                const fhirId = this.getId(sitObject.preSituation, sitObject.postSituation, index, beliefInstance.contribution);
                //create Fhir URL
                const url = this.showFullUrl(fhirId);
                //if it does find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirForecastEffect(recId, url, fhirId, sitObject, beliefInstance, index, patient));
                } else {
                    //if it already exits, add the MedicationRequest url to the instance
                    this.map.get(url).addMedicationRequestUrl(recId);
                }
            }
        };
    }

    getResourcesList() {
        //map values to array
        return Array.from(this.map.values());
    }
}
class FhirMedicationRequestResources {

    /**
     * 
     * @param {Map<string, FhirForecastEffect>} map Create a Map of  resources
     */
    constructor(map) {
        //Map containing medication resources
        this.map = map;
    }

        /**
     * 
     * @param {object} tmrRecObject TMR-based recomendation in JSON notation
     */
    showFullUrl(tmrRecObject) {
        return 'http://anonymous.org/MedicationRequest/' + String(tmrRecObject.id).slice(26);
    }

    getId(preSituation, postSituation, beliefIndex, contribution) {
        return String(preSituation) + '2' + String(postSituation) + ((beliefIndex === 0) ? 'M' : 'S') + contribution.charAt(0);
    }

    getSituations(situationTypeList) {

        let preSit = JSON.parse(situationTypeList[0]);
        let postSit = JSON.parse(situationTypeList[1]);

        //check it is not the other way around
        if (String(postSit.type) === 'hasTransformableSituation') {
            //copy not shared
            let temp = JSON.parse(JSON.stringify(postSit));
            postSit = preSit;
            preSit = temp;
        }

        return { preSituation: preSit, postSituation: postSit };
    }

     showFullUrl(fhirId) {
        return 'http://anonymous.org/ForecastEffect/' + fhirId;
    }

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return ({recId, beliefInstance, index, patient}) => {
            //if not of type undefined, act on it
            if (beliefInstance) {
                const sitObject = this.getSituations(beliefInstance.situationTypes);
                const fhirId = this.getId(sitObject.preSituation, sitObject.postSituation, index, beliefInstance.contribution);
                //create Fhir URL
                const url = this.showFullUrl(fhirId);
                //if it does find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirForecastEffect(recId, url, fhirId, sitObject, beliefInstance, index, patient));
                } else {
                    //if it already exits, add the MedicationRequest url to the instance
                    this.map.get(url).addMedicationRequestUrl(recId);
                }
            }
        };
    }

    getResourcesList() {
        //map values to array
        return Array.from(this.map.values());
    }
}

//use case
const TMRobject = {
    "id": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
    "text": "clinician should avoid recommending administration of Beta Agonist bronchodilators to patients with cardiovascular disease",
    "motivation": "none",
    "derivedFrom": "GOLD COPD 2017",
    "suggestion": "nonrecommend",
    "careActionType": {
        "id": "http://anonymous.org/data/DrugCatBetaAgonist",
        "code": "DrugCatBetaAgonist",
        "display": "administration of Beta Agonist bronchodilator",
        "requestType": 0,
        "drugLabel": "BetaAgonist"
    },
    "causationBeliefs": [
        {
            "id": "http://anonymous.org/data/CBBetaAgonistIncLowRiskCdr",
            "contribution": "negative",
            "probability": "always",
            "evidence": "High Level",
            "author": "JDA",
            "transition": {
                "id": "http://anonymous.org/data/TrIncLowRiskCdr",
                "effect": "increase",
                "property": {
                    //added on 22/09/20202. TODO: add to all elements PRoperty
                    "id": "http://anonymous.org/data/PropCrd",
                    "display": "risk of having cardiac rhythm disturbances",
                    "code": "Crd"
                },
                "situationTypes": [
                    {
                        "id": "http://anonymous.org/data/SitLowRiskCrd",
                        "type": "hasTransformableSituation",
                        "value": {
                            "code": "SitLowRiskCrd",
                            "display": "a low risk of having cardiac rhythm disturbances"
                        }
                    },
                    {
                        "id": "http://anonymous.org/data/SitMildAls",
                        "type": "hasExpectedSituation",
                        "value": {
                            "code": "SitHighRiskCrd",
                            "display": "a high risk of having cardiac rhythm disturbances"
                        }
                    }
                ]
            }
        }
    ]
};

const TMRobject2 = {
    "id": "http://anonymous.org/data/RecCOPD-LabaDecLowRiskCdrShouldnot",
    "text": "clinician should avoid recommending administration of Beta Agonist bronchodilators to patients with cardiovascular disease",
    "motivation": "none",
    "derivedFrom": "GOLD COPD 2017",
    "suggestion": "nonrecommend",
    "careActionType": {
        "id": "http://anonymous.org/data/DrugTLaba",
        "code": "DrugTLaba",
        "display": "administration of LABA",
        "requestType": 0,
        "drugLabel": "LABA"
    },
    "causationBeliefs": [
        {
            "id": "http://anonymous.org/data/CBBetaAgonistDecLowRiskCdr",
            "contribution": "negative",
            "probability": "always",
            "evidence": "High Level",
            "author": "JDA",
            "transition": {
                "id": "http://anonymous.org/data/TrIncLowRiskCdr",
                "effect": "increase",
                "property": {
                    "id": "http://anonymous.org/data/PropCrd",
                    "display": "risk of having cardiac rhythm disturbances",
                    "code": "Crd"
                },
                "situationTypes": [
                    {
                        "id": "http://anonymous.org/data/SitLowRiskCrd",
                        "type": "hasTransformableSituation",
                        "value": {
                            "code": "SitLowRiskCrd",
                            "display": "a low risk of having cardiac rhythm disturbances"
                        }
                    },
                    {
                        "id": "http://anonymous.org/data/SitMildAls",
                        "type": "hasExpectedSituation",
                        "value": {
                            "code": "SitHighRiskCrd",
                            "display": "a high risk of having cardiac rhythm disturbances"
                        }
                    }
                ]
            }
        }
    ]
};

exports.getMedication = tmrRec => new FhirMedication(tmrRec.careActionType);

exports.getCondition = (tmrRec, index) => new FhirCondition(tmrRec.situationTypes);
exports.getForecastEffect = (tmrRecId, beliefInstance, beliefIndex, patient) => new FhirMedication(tmrRecId, beliefInstance, beliefIndex, patient);
exports.getMedicationRequest = RecObject => {
    return new FhirMedication(RecObject.careActionType);
};
exports.getFhirServiceRequest = RecObject => {
    return new FhirMedication(RecObject.careActionType);
};
exports.getDetectedIssue = RecObject => {
    return new FhirMedication(RecObject.careActionType);
};
exports.getCarePlan = RecObject => {
    return new FhirMedication(RecObject.careActionType);
};

exports.example = TMRobject;
exports.example1 = TMRobject;
exports.example2 = TMRobject2;