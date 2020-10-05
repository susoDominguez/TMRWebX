'use strict';

//TODO: entries array to string or JSON
function Card( options = {} ) {
    
    const {
        uuid = 'CIG-00000000000000' ,
        patient = 'dummy' ,
        summary = 'mitigated COPD proposed care plan' ,
        labelSource = 'GOLD 2017 COPD Guideline' ,
        labelSuggestions = 'COPD care plan decision support' ,
        actionDescription = 'Update COPD care plan' ,
        resourceId = 'COPDbundle' ,
        birthDate = '1978-03-17'
    } = options; //default values for COPD

    this.patient = patient;
    let patientUrl = 'http://acme.com/Patient/' + patient;
    this.birthDate = birthDate;
    this.entries = [
        {
            fullUrl: patientUrl,
            resource: {
                resourceType: "Patient",
                id: this.patient},
                active: true,
                birthDate: birthDate
        }
    ];

    //only property of Card
    this.toString = ( ()=> `{
        "cards": [
            {
                "summary": "${summary}",
                "indicator": "info",
                "source": {
                    "label": "${labelSource}"
                },
                "suggestions": [
                    {
                        "label": "${labelSuggestions}",
                        "uuid": "${uuid}",
                        "actions": [
                            {
                                "type": "update",
                                "description": "${actionDescription}",
                                "resource": {
                                    "resourceType": "Bundle",
                                    "id": "${resourceId}",
                                    "type": "collection",
                                    "entry": ${this.entries.toJSON}
                                }
                            }
                        ]
                    }
                ],
                "selectionBehaviour": "at-most-one"
            }
        ]
    }`);
    
};

/**
 * Object with arguments required to create a new Card object
 */
const cardParams = ({
    uuid : undefined ,
    patient : undefined ,
    summary : undefined ,
    labelSource : undefined ,
    labelSuggestions : undefined ,
    actionDescription : undefined ,
    resourceId : undefined ,
    birthDate : undefined
});

const requestMap = new Map([[0,'MedicationRequest'], [1,'ServiceRequest'], [2, 'MedicationRequest']]);//[2,ImmunizationRecommendation]


//PROTOTYPE FUNCTIONS SHARED BY MORE THAN ONE CLASS 

/**
 * 
 * @param {Array} param0 array containing pre and post situation in no specific order
 */
function getSituations([sitA, sitB]) {
    return  (String(sitA.type) === 'hasTransformableSituation') ? { preSituation: sitA, postSituation: sitB } : { preSituation: sitB, postSituation: sitA };    
}

function getResourceList() {
    return Array.from(this.map.values());
}

function getId(preSituation, postSituation, beliefIndex, contribution) {
    return String(preSituation) + '2' + String(postSituation) + ((beliefIndex === 0) ? 'M' : 'S') + contribution.charAt(0);
}

function add_MainCond_and_EffectList(causationBeliefs) {

   let condition, forecastEffect;

   if(Array.isArray(causationBeliefs)) {
           causationBeliefs.map( function( {id, contribution, probability, evidence, author, transition, situationTypes}, index){
             //extract pre and post situation objects
             let {preSituation, postSituation} = this.getSituations(situationTypes);
                condition = 'Condition/' + preSituation;
                //add condition id if this is the main effect (side effects conditions can be fetched via forecast effects)
                if(index === 0) this._conditionList.push({ reference: condition });

                forecastEffect = 'ForecastEffect/' + this.getId(preSituation, postSituation, index);
                //add forecastEffect id
                this._forecastEffectList.push({ reference: forecastEffect });
            }, this);
    } else {
        //TODO: ERROR handling
    }
}

function toString() {
    JSON.stringify(this.toJSON());
}

//////RESOURCE CLASSES

//class to represent Medication FHIR resources
class FhirMedication {

    /**
     * 
     * @param {string} url 
     * @param {object} careActionTypeObject 
     */
    constructor(url, {id, code, display, requestType , drugLabel}) {

        //resource id
        this._id = String(code);
        //drug URL
        this._codingSystem = String(id);
        //drug label
        this._codingCode = String(drugLabel);
        //display of care action label
        this._codingDisplay = String(display);
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
    toJSON = function() {
        return ({
            fullUrl: this._fullUrl,
            resource:
            {
                resourceType: "Medication",
                id: this._id,
                code: this._codeObject
            }
        });
    }
    
}
FhirMedication.prototype.toString = toString;

//class to represent Condition FHIR resources
class FhirCondition {

    /**
     * @param {string} url
     * @param {object} situationType 
     * @param {string} patient 
     */
    constructor(url, {id, type, value : {code, display}}, patient) {
        this._id = String(code);
        this._codingSystem = String(id);
        this._codingDisplay = String(display);
        this._fullUrl = url;
        this._patient = patient;
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

FhirCondition.prototype.toString = toString;

//class to represent ForecastEffect FHIR resources
class FhirForecastEffect {

    constructor(recId, url, fhirID, request, {id, contribution, probability, evidence, author, transition, preSituation, postSituation}, index, patient) {

        this._medicationRequestList = [];

        //constants of the class forecastEffect
        let main = 'main-effect';
        let side = 'side-effect';
        let positive = 'adverse-event';
        let negative = 'therapeutic-event';

        
        this.addRequestUrl(recId, request);

        //temp let containing identifier of this ForecastEffect instance
        this._id = fhirID;
        this._fullUrl = url;
        this._effectType = (index === 0) ? main : side;
        this._eventType = (String(contribution) == 'positive') ? positive : negative;
        this._probability = String(probability);
        this._evidenceLevel = String(evidence);
        this._expectedCodingSystem = String(postSituation.id);
        this._expectedCodingCode = String(postSituation.value.code);
        this._expectedCodingDisplay = String(postSituation.value.display);
        this._propCodingSystem = String(transition.property.id);
        this._propCodingCode = String(transition.property.code);
        this._propCodingDisplay = String(transition.property.display);
        this._degree = String(transition.effect);
        this._patient = patient;
        this._condition = 'Condition/' + preSituation.value.code;
    }
    

    addRequestUrl(recId, request) {
        let medReq = requestMap.get(request) + '/' + recId.slice(26);
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
FhirForecastEffect.prototype.toString = toString;

class FhirMedicationRequest {

    /**
     * @param {string} url FHIR entry element unique ULR
     * @param {string} fhirId FHIR resource identifier
     * @param {object} recObject TMR recomendation object
     * @param {object} interactions TMR interactions object
     * @param {string} patient patient id
     */
    constructor(url, fhirId, {id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}, interactions, patient) {

        this._conditionList = [];
        this._forecastEffectList = [];
        this._detectedIssueList = [];

        this._fullUrl = url;
        this._id = fhirId;
        this._cigUri = String(derivedFrom);
        this._patient = patient;
        this._medication = 'Medication/' + String(careActionType.code);
        this._doNotRecommend = String(suggestion) === 'nonrecommend';
        //create list of Conditions and ForecastEffects -both have the same number of resources
        this.add_MainCond_and_EffectList(causationBeliefs);
        this.addDetectedIssues(interactions);
    }

    /**
     *  
     * @param {Array} interactionList 
     */
    addDetectedIssues(interactionList) {

        if(Array.isArray(interactionList)) {

            const prefix = 'DetectedIssue/';
            //reconstruct TMR URI for recommendation
            const tmrId = ('http://anonymous.org/data/' + this.id) ;
            let refId;

            interactionList.map( ({type, interactionNorms}, index) => {
                 refId = prefix + type + index ;

                if(Array.isArray(interactionNorms)) {

                    //add each norm w/interaction to the detectedIssue List
                    interactionNorms.map( ( normObject ) => {

                        if(tmrId === String(normObject.recId)){
                            this._detectedIssueList.push({
                                reference: refId
                            });
                        }

                    }, this);

                } else {
                    //TODO: throw error
                }
            }, this);       
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
                doNotPerform: this._doNotRecommend,
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
FhirMedicationRequest.prototype.toString = toString;
FhirMedicationRequest.prototype.getSituations = getSituations;
FhirMedicationRequest.prototype.getId = getId;
FhirMedicationRequest.prototype.add_MainCond_and_EffectList = add_MainCond_and_EffectList;

class FhirServiceRequest {

    /**
     * @param {string} url FHIR entry element unique ULR
     * @param {string} fhirId FHIR resource identifier
     * @param {object} recObject TMR recomendation object
     * @param {string} patient patient id
     */
    constructor(url, fhirId, {id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}, patient) {

        this._conditionList = [];
        this._forecastEffectList = [];

        this._fullUrl = url;
        this._id = fhirId;
        this._cigUri = String(derivedFrom);
        this._patient = patient;
        this._medication = 'ServiceRequest/' + String(careActionType.code);
        this._doNotRecommend = String(suggestion) === 'nonrecommend';
        //create list of Conditions and ForecastEffects -both have the same number of resources
        this.add_MainCond_and_EffectList(causationBeliefs);
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
                doNotPerform: this._doNotRecommend,
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

FhirServiceRequest.prototype.getSituations = getSituations;
FhirServiceRequest.prototype.getId = getId;
FhirServiceRequest.prototype.add_MainCond_and_EffectList = add_MainCond_and_EffectList;
FhirServiceRequest.prototype.toString = toString;

/*
class FhirDetectedIssue {

    
    this._fullUrl;
    this._issueSystem;
    this._issueCode;
    this._issueDisplay;
    this._implicatedList = [];
    this._mitigation;


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
*/
/////////////////////// Classes holding resource classes

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
    showFullUrl = ({id, code, display, requestType , drugLabel}) => 'http://anonymous.org/Medication/' + code;

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return ({id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}) => {
            
            if (careActionType) {
                //create Fhir URL
                const url = this.showFullUrl(careActionType);
                //if it does not find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirMedication(url, careActionType));
                }
            }
        };
    }
}

MedicationResources.prototype.getResourceList = getResourceList;

/**
 * 
 */
class ConditionResources {

    /**
     * 
     * @param {Map<string, FhirCondition>} map Create a Map of condition resources
     */
    constructor(map) {
        //Map containing  resources
        this.map = map;
    }

    showFullUrl({id, type, value : {code, display}}) {

        return 'http://anonymous.org/Condition/' + String(code);
    }

    add() {
        //obtain argument from the calling function
        return ({id, contribution, probability, evidence, author, transition, situationTypes}, patient) => {

            //if not undefined, act on it
            if (situationTypes) {
                let preSit = this.getSituations(situationTypes).preSituation;
                //create Fhir URL
                const url = this.showFullUrl(preSit);
                //if it doesnt find key, add new Condition using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirCondition(url, preSit, patient));
                }
            }
        };
    }
}
//add getSituations to prototype of ConditionResources
ConditionResources.prototype.getSituations = getSituations;
ConditionResources.prototype.getResourceList = getResourceList;

class FhirForecastEffectResources {

    /**
     * 
     * @param {Map<string, FhirForecastEffect>} map Create a Map of  resources
     */
    constructor(map) {
        //Map containing medication resources
        this.map = map;
    }

    showFullUrl(fhirId) {
        return 'http://anonymous.org/ForecastEffect/' + fhirId;
    }

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return (recId, request, {id, contribution, probability, evidence, author, transition, situationTypes}, index, patient) => {
            //if not of type undefined, act on it
            if ( id && contribution && probability && evidence && transition && situationTypes) {
                const {preSituation, postSituation} = this.getSituations(situationTypes);
                const fhirId = this.getId(preSituation, postSituation, index, contribution);
                //create Fhir URL
                const url = this.showFullUrl(fhirId);
                //if it doesnt find key, add new ForecastEffect using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirForecastEffect(recId, url, fhirId, request, {id, contribution, probability, evidence, author, transition, preSituation, postSituation}, index, patient));
                } else {
                    //if it already exits, add the MedicationRequest url to the instance
                    this.map.get(url).addRequestUrl(recId, request);
                }
            } else {
                //TODO: error checking
            }
        };
    }
}
//adding function getSituations to class FhirForecastEffect
FhirForecastEffectResources.prototype.getSituations = getSituations;  
FhirForecastEffectResources.prototype.getResourceList = getResourceList;
FhirForecastEffectResources.prototype.getId = getId;


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
    showFullUrl(fhirId) {
        return 'http://anonymous.org/MedicationRequest/' + fhirId;
    }

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return ({id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}, interactions, patient) => {

            if('MedicationRequest' === requestMap.get(careActionType.requestType) ){
                const fhirId = String(id).slice(26);
                //create Fhir URL
                const url = this.showFullUrl(fhirId);
                //if it does find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirMedicationRequest(url, fhirId, {id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}, interactions, patient));
                } 
            }
        
        };
    }
}

//adding function getSituations to class FhirForecastEffect
FhirMedicationRequestResources.prototype.getSituations = getSituations;  
FhirMedicationRequestResources.prototype.getResourceList = getResourceList;
FhirMedicationRequestResources.prototype.getId = getId;

class FhirServiceRequestResources {

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
    showFullUrl(fhirId) {
        return 'http://anonymous.org/ServiceRequest/' + fhirId;
    }

    /**
     * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
     */
    add() {
        //obtain argument from the calling function
        return ({id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}, patient) => {

            if('ServiceRequest' === requestMap.get(careActionType.requestType) ){
                const fhirId = String(id).slice(26);
                //create Fhir URL
                const url = this.showFullUrl(fhirId);
                //if it does find key, add new medication using the care action type
                if (!this.map.has(url)) {
                    this.map.set(url, new FhirServiceRequest(url, fhirId, {id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}, patient));
                } 
            }
        
        };
    }
}

//adding function getSituations to class FhirForecastEffect
FhirServiceRequestResources.prototype.getSituations = getSituations;  
FhirServiceRequestResources.prototype.getResourceList = getResourceList;
FhirServiceRequestResources.prototype.getId = getId;

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

exports.cardParams =  cardParams;
exports.createCard = (options) => new Card(options);
exports.getMedication = (url, {id, text, motivation, derivedFrom, suggestion, careActionType, causationBeliefs}) => new FhirMedication(url, careActionType);
exports.createMedicationList = 
exports.getCondition = (url, {id, contribution, probability, evidence, author, transition, situationTypes}, index) => new FhirCondition(url, situationTypes);
exports.getForecastEffect = (tmrRecId, beliefInstance, beliefIndex, patient) => new FhirMedication(tmrRecId, beliefInstance, beliefIndex, patient);
/*exports.getMedicationRequest = RecObject => {
    return new FhirMedication(RecObject.careActionType);
};*/

/*
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
*/