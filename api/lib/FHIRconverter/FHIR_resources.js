'use strict';

//class to represent Medication FHIR resources
class FhirMedication {

     /**
     * 
     * @param {object} careActionTypeObject 
     */
    constructor(careActionTypeObject) {

        this._medicationUrlPrefix = 'http://anonymous.org/Medication/';
        this._id = String(careActionTypeObject.code);
        this._codingSystem = String(careActionTypeObject.id);
        this._codingCode = String(careActionTypeObject.drugLabel);
        this._codingDisplay = String(careActionTypeObject.display);
        this._fullUrl = this.showFullUrl(careActionTypeObject);
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
    * 
    * @param {object} careActionTypeObject the care action type object (potentially a FHIR resource) to be compared with the resource for identical identifiers
    * 
    * @returns {boolean}
    */
    compareFullUrlWith(careActionTypeObject) {
        let lhs = this.showFullUrl(careActionTypeObject) ;
        let rhs = String(this._fullUrl);
        return lhs === rhs;
    }

    /**
     * 
     * @param {object} careActionTypeObject an intance object of a TMR-based care action type
     */
    showFullUrl(careActionTypeObject) {
        return this._medicationUrlPrefix + careActionTypeObject.code;
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
     * 
     * @param {object} situationTypeList 
     * @param {string} patient 
     */
    constructor(situationTypeList, patient) {

        this.__conditionUrlPrefix = 'http://anonymous.org/Condition/';
        const preSitObject = (String(situationTypeList[0].type) === 'hasTransformableSituation') ? situationTypeList[0] : situationTypeList[1];
        this._id = this._codingCode = String(preSitObject.value.code);
        this._codingSystem = String(preSitObject.id);
        this._codingDisplay = String(preSitObject.value.display);
        this._fullUrl = this.showFullUrl(situationTypeList);
        this._patient = 'Patient/' + (patient !== undefined) ? String(patient) : 'dummy';
        this._codeObject = {
            coding:
                [
                    {
                        system: this._codingSystem,
                        code: this._codingCode,
                        display: this._codingDisplay
                    }
                ]
        } ;
    }

    /**
     * 
     * @param {string} situationTypeList 
     * @param {object} conditionObject 
     * 
     * @returns {boolean}
     */
     compareFullUrlWith(situationTypeList) {
        return this.showFullUrl(situationTypeList) == String(this._fullUrl);
    }

    /**
     * 
     * @param {object} situationTypeList an intance object of a TMR-based care action type
     * 
     * @returns {string}
     */
    showFullUrl(situationTypeList) {

        const preSitObject = (String(situationTypeList[0].type) === 'hasTransformableSituation') ? situationTypeList[0] : situationTypeList[1];
        return this._conditionUrlPrefix + String(preSitObject.value.code) ;
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

    /**
     * 
     * @param {string} tmrRecId 
     * @param {number} beliefInstance 
     * @param {string} patient 
     *
     */
    constructor(tmrRecId, beliefInstance, beliefIndex, patient) {

        //constants of the class forecastEffect
        let main = 'main-effect' ;
        let side = 'side-effect' ;
        let positive = 'adverse-event' ;
        let negative = 'therapeutic-event' ;
        this._medRequestPrefix = 'MedicationRequest/' ;
        this._medicationRequestList = [] ;
        this._forecastEffectUrlPrefix = 'http://anonymous.org/ForecastEffect/' ;

        const sitObject = this.getSituations(beliefInstance) ;

        //temp let containing identifier of this ForecastEffect instance
        this._id = this.getId(sitObject.preSituation.value.code, sitObject.postSituation.value.code, beliefIndex, beliefInstance.contribution);
        this._fullUrl = this.showFullUrl(beliefInstance, beliefIndex) ;
        this._effectType = (beliefIndex === 0) ? main : side ;
        this._eventType = (String(beliefInstance.contribution) == 'positive') ? positive : negative ;
        this._probability = String(beliefInstance.probability) ;
        this._evidenceLevel = String(beliefInstance.evidence) ;
        this._expectedCodingSystem = String(sitObject.postSituation.id) ;
        this._expectedCodingCode = String(sitObject.postSituation.value.code) ;
        this._expectedCodingDisplay = String(sitObject.postSituation.value.display) ;
        this._propCodingSystem = String(beliefInstance.transition.property.id);
        this._propCodingCode = String(beliefInstance.transition.property.code);
        this._propCodingDisplay = String(beliefInstance.transition.property.display);
        this._degree = String(beliefInstance.transition.effect);
        this._patient = 'Patient/' + (patient !== undefined ? patient : 'dummy');
        this.medicationRequest(String(tmrRecId)) ;
        this._condition = 'Condition/' + sitObject.preSituation.value.code;
    }


    /**
     * 
     * @param {string} preSitCode 
     * @param {string} postSitCode 
     * @param {number} beliefIndex 
     * @param {string} contribution 
     */
    getId(preSitCode, postSitCode, beliefIndex, contribution){
        return String(preSitCode) + '2' + String(postSitCode) + ((beliefIndex === 0) ? 'M' : 'S') + String(contribution).charAt(0);
    }
    /**
     * 
     * @param {object} beliefInstance 
     * @param {number} beliefIndex 
     */
    showFullUrl(beliefInstance, beliefIndex) {

        const sitsObject = this.getSituations(beliefInstance.transition.situationTypes);

        return  'http://anonymous.org/ForecastEffect/' + this.getId(sitsObject.preSituation.value.code, sitsObject.postSituation.value.code, beliefIndex, beliefInstance.contribution) ;
    }

    /**
     * 
     * @param {object} beliefInstance 
     * 
     * @returns {boolean} 
     */
    compareFullUrlWith(beliefInstance, beliefIndex) {
        let lhs = this.showFullUrl(beliefInstance, beliefIndex);
        let rhs = String(this._fullUrl);
        return lhs == rhs;
    }

    /**
     * 
     * @param {object} situationTypeList 
     */
    getSituations(situationTypeList){
        let preSit, postSit;

        preSit = JSON.parse(situationTypeList[0]) ;
        postSit = JSON.parse(situationTypeList[1]) ;

        //check assignment is not the other way
        if (String(postSit.type) === 'hasTransformableSituation') {
            //copy not shared
            let temp = JSON.parse(JSON.stringify(postSit)) ;
            postSit = preSit ;
            preSit = temp;
        } 

        return {preSituation: preSit, postSituation: postSit} ;
    }


    /**
     * 
     * @param {string} recId add another MedicationRequest resource to this ForecastEffect
     */
    set medicationRequest(recId) {
        this._medicationRequestList.push({ reference: (this._medRequestPrefix + recId.slice(26)) });
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

//TODO: IM HERE
class FhirMedicationRequest {


    /**
     * 
     * @param {object} recObject TMR recomendation object
     * @param {string} patient patient id
     */
    constructor(recObject, patient) {
     
        this._conditionList = [];
        this._forecastEffectList = [];
        this.__detectedIssueList = [] ;
        this._detectedIssuePrefix = 'DetectedIssue/';
        this._medRequestUrlPrefix = 'http://anonymous.org/MedicationRequest/';
        this._fullUrl = this.showFullUrl(recObject);
        this._id = String(recObject.id).slice(26);
        this._cigUri = String(recObject.derivedFrom);
        this._patient = 'Patient/' + (patient !== undefined) ? patient :  'dummy';
        this._medication = 'Medication/' + String(recObject.careActionType.code);
        this._doNotRecommend = String(recObject.suggestion) === 'nonrecomend';
        //create list of Conditions and ForecastEffects -both have the same number of resources
        this.createResourcesList(recObject.causationBeliefs);
    }


    /**
     * 
     * @param {Array<object>} causationBeliefList list of causation beliefs in a given TMR-based recomendation
     * 
     * @returns {object} an object of form {conditionList:: Array<object>, forecastEffectList:: Array<object>}
     */
    createResourcesList(causationBeliefList) {

        //add a reference to both lists above
        for (let i = 0; i < causationBeliefList.length; i++) {

            //extract pre and post situation objects
            let preSit, postSit;
            //retrieve causation belief
            let cb = causationBeliefList[i];

            //retrieve condition id
            let preSituation = String(cb.situationTypes[0].value.code);
            const condition = 'Condition/' + preSituation;
            //retrieve forecastEffectId
            let postSituation = cb.situationTypes[1].value.code;
            let forecastEffectId = preSituation + '2' + postSituation + ((i === 0) ? 'M' : 'S') + cb.contribution.charAt(0);
            const forecastEffect = 'ForecastEffect/' + forecastEffectId;
            //add condition id
            this._conditionList.push({ reference: condition });
            //add forecastEffect id
            this._forecastEffectList.push({ reference: forecastEffect });
        }
    }

    /**
    * 
    * @param {object} tmrRecObject 
    * 
    * @returns {boolean} 
    */
    compareFullUrlWith(tmrRecObject) {
        let lhs = this.showFullUrl(tmrRecObject);
        let rhs = String(this._fullUrl);
        return lhs == rhs;
    }

    /**
     * 
     * @param {object} tmrRecObject TMR-based recomendation in JSON notation
     */
    showFullUrl(tmrRecObject) {
        return this._medRequestUrlPrefix + String(tmrRecObject.id).slice(26);
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

class FHIRserviceRequest {

     /**
     * 
     * @param {object} recObject TMR recomendation object
     * @param {string} patient patient id
     */
    constructor(recObject, patient) {
     
        this._conditionList = [];
        this._forecastEffectList = [];

        const careActionTypeObject = recObject.careActionType;
        this._serviceSystem = String(careActionTypeObject.id);
        this._serviceCode = String(careActionTypeObject.drugLabel);
        this._serviceDisplay = String(careActionTypeObject.display);
        this._medRequestUrlPrefix = 'http://anonymous.org/MedicationRequest/';
        this._fullUrl = this.showFullUrl(recObject);
        this._id = String(recObject.id).slice(26);
        this._cigUri = String(recObject.derivedFrom);
        this._patient = 'Patient/' + (patient !== undefined) ? patient :  'dummy';
        this._medication = 'Medication/' + String(recObject.careActionType.code);
        this._doNotRecommend = String(recObject.suggestion) === 'nonrecomend';
        //create list of Conditions and ForecastEffects -both have the same number of resources
        this.createResourcesList(recObject.causationBeliefs);
    }

    /**
     * 
     * @param {object} tmrRecObject TMR-based recomendation in JSON notation
     */
    showFullUrl(tmrRecObject) {
        return this._medRequestUrlPrefix + String(tmrRecObject.id).slice(26);
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
        } ;
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
        } ;
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
        } ;
    }
}