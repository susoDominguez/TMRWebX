"use strict";

const {assert} = require('@sindresorhus/is');

//TODO: entries array to string or JSON
function Card(options = {}) {
  const {
    uuid = "CIG-00000000000000",
    patient = "dummy",
    summary = "mitigated COPD proposed care plan",
    labelSource = "GOLD 2017 COPD Guideline",
    labelSuggestions = "COPD care plan decision support",
    actionDescription = "Update COPD care plan",
    resourceId = "COPDbundle",
    birthDate = "1978-03-17",
  } = options; //default values for COPD

  this.patient = patient;
  let patientUrl = "http://acme.com/Patient/" + patient;
  this.birthDate = birthDate;
  this.entries = [
    {
      fullUrl: patientUrl,
      resource: {
        resourceType: "Patient",
        id: this.patient,
      },
      active: true,
      birthDate: birthDate,
    },
  ];

  //only property of Card
  this.toString = () => `{
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
    }`;
}

/**
 * Object with arguments required to create a new Card object
 */
const cardParams = {
  uuid: undefined,
  patient: undefined,
  summary: undefined,
  labelSource: undefined,
  labelSuggestions: undefined,
  actionDescription: undefined,
  resourceId: undefined,
  birthDate: undefined,
};

/**
 * constant of the FHIR syntax
 */
const uriPrefix = "http://anonymous.org/";
const med_ID = "Medication",
  cond_ID = "Condition",
  effect_ID = "ForecastEffect",
  medReq_ID = "MedicationRequest",
  servReq_ID = "ServiceRequest",
  interact_ID = "interactions",
  detecIs_ID = "DetectedIssue",
  carePlan_ID = "CarePlan";
/**
 * types of request: MEdication, service or vaccination
 */
const requestMap = new Map([
  [0, medReq_ID],
  [1, servReq_ID],
  [2, medReq_ID],
]); //[2,ImmunizationRecommendation]

const altInter = "alternative",
  contrInter = "contradiction",
  repairInter = "repairable",
  repetInter = "repetition";

  //mitigation codes for contradiction
const contrMitStopped = '13', contrMitAlt = 'ALTHRPYMIT', contrMitRep = 'INVEFFCTMIT';

//interaction types
const interactionCodes = new Map([
  [
    altInter,
    {
      interaction: [{
            system: "http://anonymous.org/CodeSystem/interactions",
            code: "ALTHRPY",
            display: "Alternative Therapies With Same Intended Effect" 
      }],
      mitigation: [
        { 
            action: {
                coding: [
                    {
                    system: "http://anonymous.org/CodeSystem/interactions",
                    code: "NOTREQ",
                    display: "Mitigation Not Required",
                    }
                ]
            }
        }
      ]
    }
  ],
  [
    contrInter,
    {
      interaction: [{
        system: "http://terminology.hl7.org/CodeSystem/v3-ActCode",
        code: "DACT",
        display: "Drug Action Detected Issue",
      }],
      mitigation: [
        { 
            action: {
                coding: [
                            {
                             system: "http://terminology.hl7.org/CodeSystem/v3-ActCode",
                             code: contrMitStopped,
                            display: "Stopped Concurrent Therapy",
                            }
                        ]
                    }
         },
        {
            action: {
                coding: [
                    {
                        system: "http://terminology.hl7.org/CodeSystem/interactions",
                        code: contrMitAlt,
                        display: "Selected Alternative Therapy With Same Intended Effect",
                    }
                ]
            }
        },
        {
            action: {
                coding: [
                    {
                        system: "http://terminology.hl7.org/CodeSystem/interactions",
                        code: contrMitRep,
                        display: "Adverse Effect Repaired By Drug Action with Inverse Effect",
                    }
                ]
            }
        }
      ]
    }
  ],
  [
    repetInter,
    {
      interaction: [
            {
            system: "http://terminology.hl7.org/CodeSystem/v3-ActCode",
            code: "DUPTHPY",
            display: "Duplicate Therapy Alert",
            }
        ],
      mitigation: [
        { 
          action: {
                coding: [
                    {
                        system: "http://terminology.hl7.org/CodeSystem/v3-ActCode",
                        code: "13",
                        display: "Stopped Concurrent Therapy",
                    }
                ]
            }
        }
      ]
    }
  ],
  [
      repairInter, 
    {
      interaction: 
      [
          {
            system: "http://anonymous.org/codeSystem/interactions",
            code: "INVEFFCT",
            display: "Adverse and therapeutic therapies with inverse effect",
          }
      ],
      mitigation:
      [
        {
          action: {
            coding: [
                {
                system: "http://anonymous.org/codeSystem/interactions",
                code: "NOTREQ",
                display: "Mitigation Not Required",
                }
            ]
          }
        }
      ]
   }
  ]
]);

//validating parameters
//this one is when parametr is undefined the default value is an error object
const required = (name, className) => {
    throw new Error(`Parameter ${name} is required in class ${className}`);
};
//PROTOTYPE FUNCTIONS SHARED BY MORE THAN ONE CLASS

/**
 *
 * @param {Array} param0 array containing pre and post situation in no specific order
 */
function getSituations([sitA = required('situationA', this.constructor.name), 
                        sitB = required('situationA', this.constructor.name)]) {

      //assert they are objects
      assert.plainObject(sitA);
      assert.plainObject(sitB);
     if(!('type' in sitA)) throw new Error('propery type is missing in situationType');

  return String(sitA.type) === "hasTransformableSituation"
    ? { preSituation: sitA, postSituation: sitB }
    : { preSituation: sitB, postSituation: sitA };
}

function getResourceList() {
  return Array.from(this.map.values());
}

function getId(
    preSituation = required('preSituation', this.constructor.name),
    postSituation = required('postSituation', this.constructor.name),
    beliefIndex = required('beliefIndex', this.constructor.name),
    contribution = required('contribution', this.constructor.name)) {
  return String(
    preSituation.value.code +
      "2" +
      postSituation.value.code +
      (beliefIndex === 0 ? "M" : "S") +
      contribution.charAt(0)
  );
}

function validateCBSchema(causationBelief){

  const cbPropList = ['id', 'contribution', 'probability', 'evidence', 'author', 'transition'],
    trPropList = ['id', 'effect', 'property', 'situationTypes'],
    prPropList = ['id', 'display', 'code'], sitPropList = ['id', 'type', 'value', 'value.code', 'value.display'];

  if(!cbPropList.every( prop => prop in causationBelief )) throw new Error(`property ${prop} is missing in causationBelief with id ${causationBelief.id}`);
  if(!trPropList.every( prop => prop in causationBelief.transition )) throw new Error(`property ${prop} is missing in transition with causationBelief id ${causationBelief.id}.`);
  if(!prPropList.every( prop => prop in causationBelief.property )) throw new Error(`property ${prop} is missing in property with causationBelief id ${causationBelief.id}.`);
  let situationTypes = causationBelief.transition.situationTypes;
  if(!situationTypes || !Array.isArray(situationTypes)) throw new Error(`situationTypes is not an array in causationBelief ${causationBelief.id}.`)
  if(!situationTypes[0] || !situationTypes[1]) throw new Error(`situation is missing from CausationBelief ${causationBelief.id}.`);
  if(!sitPropList.every( prop => prop in situationTypes[0])) throw new Error(`property ${prop} is missing in situation with causationBelief id ${causationBelief.id}.`);
  if(!sitPropList.every( prop => prop in situationTypes[1])) throw new Error(`property ${prop} is missing in situation with causationBelief id ${causationBelief.id}.`);
}

function add_MainCond_and_EffectList(causationBelief, index) {

  let condition, forecastEffect;

      //extract pre and post situation objects
        let { preSituation, postSituation } = this.getSituations(
          causationBelief.transition.situationTypes
        );
        condition = cond_ID + "/" + preSituation.value.code;
        //add condition id if this is the main effect (side effects conditions can be fetched via forecast effects)
        if (index === 0) this._conditionList.push({ reference: condition });

        forecastEffect =
          effect_ID +
          "/" +
          this.getId(preSituation, postSituation, index, causationBelief.contribution);
        //add forecastEffect id
        this._forecastEffectList.push({ reference: forecastEffect });
}

function toString() {
  return JSON.stringify(this.toJSON(), null, '  ');
}

//////RESOURCE CLASSES

//class to represent Medication FHIR resources
class FhirMedication {
  /**
   *
   * @param {string} url
   * @param {object} careActionTypeObject
   */
  constructor(url, { id, code, display, requestType }) {
    //resource id
    this._id = String(id).slice(26);
    //drug URL
    this._codingSystem = String(id);
    //drug label
    this._codingCode = code;
    //display of care action label
    this._codingDisplay = String(display);
    this._fullUrl = url;
    this._codeObject = {
      coding: [
        {
          system: this._codingSystem,
          code: this._codingCode,
          display: this._codingDisplay,
        },
      ],
    };
  }

  /**
   * Transforms the instance into a JS object
   *
   * @returns {Object}
   */
  toJSON = function () {
    return {
      fullUrl: this._fullUrl,
      resource: {
        resourceType: med_ID,
        id: this._id,
        code: this._codeObject,
      },
    };
  };
}
FhirMedication.prototype.toString = toString;

//class to represent Condition FHIR resources
class FhirCondition {
  /**
   * @param {string} url
   * @param {object} situationType
   * @param {string} patient
   */
  constructor(url, { id, type, value: { code, display } }, patient) {
    this._id = String(code);
    this._codingSystem = String(id);
    this._codingDisplay = String(display);
    this._fullUrl = url;
    this._patient = patient;
    this._codeObject = {
      coding: [
        {
          system: this._codingSystem,
          code: this.id,
          display: this._codingDisplay,
        },
      ],
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
      resource: {
        resourceType: cond_ID,
        id: this._id,
        code: this._codeObject,
        subject: {
          reference: this._patient,
        },
      },
    };
  }
}

FhirCondition.prototype.toString = toString;

//class to represent ForecastEffect FHIR resources
class FhirForecastEffect {
  constructor(
    recId,
    url,
    fhirID,
    request,
    {
      id,
      contribution,
      probability,
      evidence,
      author,
      transition,
      preSituation,
      postSituation,
    },
    index,
    patient
  ) {
    this._requestList = [];

    //constants of the class forecastEffect
    const main = "main-effect";
    const side = "side-effect";
    const negative = "adverse-effect";
    const positive = "therapeutic-effect";

    this.addRequestUrl(recId, request);

    //temp let containing identifier of this ForecastEffect instance
    this._id = fhirID;
    this._fullUrl = url;
    this._effectType = index === 0 ? main : side;
    this._eventType = String(contribution) == "positive" ? positive : negative;
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
    this._condition = cond_ID + "/" + preSituation.value.code;
  }

  addRequestUrl(recId, request) {
    let medReq = requestMap.get(request) + "/" + recId.slice(26);
    this._requestList.push({ reference: medReq });
  }

  toJSON() {
    return {
      fullUrl: this._fullUrl,
      resource: {
        resourceType: effect_ID,
        id: this._id,
        typeOfEffect: this._effectType,
        typeOfEvent: this._eventType,
        subject: {
          reference: this._patient,
        },
        appliesTo: {
          careActionInstance: this._requestList,
          conditionAddressed: {
            reference: this._condition,
          },
        },
        expectedOutcomeCode: {
          coding: [
            {
              system: this._expectedCodingSystem,
              code: this._expectedCodingCode,
              display: this._expectedCodingDisplay,
            },
          ],
        },
        targetMeasurement: {
          measuredProperty: {
            coding: [
              {
                system: this._propCodingSystem,
                code: this._propCodingCode,
                display: this._propCodingDisplay,
              },
            ],
          },
          degreeOfChange: this._degree,
        },
        probability: this._probability,
        evidence: this._evidenceLevel,
      },
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
  constructor(
    url,
    fhirId,
    {
      id,
      text,
      motivation,
      derivedFrom,
      suggestion,
      careActionType,
      causationBeliefs,
    },
    interactions,
    patient
  ) {
    this._conditionList = [];
    this._forecastEffectList = [];
    this._detectedIssueList = [];

    this._fullUrl = url;
    this._id = fhirId;
    this._cigUri = String(derivedFrom);
    this._patient = patient;
    this._medication = med_ID + "/" + String(careActionType.id).slice(26);
    this._doNotRecommend = String(suggestion) === "nonrecommend";
    //create list of Conditions and ForecastEffects -both have the same number of resources
     //validate schema
     for (let index = 0; index < causationBeliefs.length; index++) {
      const causationBelief = causationBeliefs[index];
      this.validateCBSchema(causationBelief);
      this.add_MainCond_and_EffectList(causationBelief);
    }
    this.addDetectedIssues(interactions);
  }

  /**
   *
   * @param {Array} interactionList           TODO: check algorithm
   */
  addDetectedIssues(interactionList) {
    if (!Array.isArray(interactionList)) throw new Error('interaction is not an array');

      //reconstruct TMR URI for recommendation
      const tmrId = uriPrefix + 'data/' + this._id;
      let refId;
      for (let index = 0; index < interactionList.length; index++) {
        const { type, interactionNorms } = interactionList[index];

        if (!Array.isArray(interactionNorms)) throw new Error('interactionNorms is not an array');

        refId = detecIs_ID + "/" + type + index;
        //for each norm, check whether they are refering to this MedicationRequest
        for (let i = 0; i < interactionNorms.length; i++) {
          const norm = interactionNorms[i];
          if (tmrId === String(norm.recId)) {
            this._detectedIssueList.push({
              reference: refId,
            });
          }//end If 
        }//end For
      }//end  For
  }

  toJSON() {
    return {
      fullUrl: this._fullUrl,
      resource: {
        resourceType: medReq_ID,
        id: this._id,
        status: "active",
        intent: "plan",
        instantiatesUri: this._cigUri,
        doNotPerform: this._doNotRecommend,
        reasonReference: this._conditionList,
        "forecast-effects": this._forecastEffectList,
        medicationReference: {
          reference: this._medication,
        },
        subject: {
          reference: this._patient,
        },
        detectedIssue: this._detectedIssueList,
      },
    };
  }
}
FhirMedicationRequest.prototype.toString = toString;
FhirMedicationRequest.prototype.getSituations = getSituations;
FhirMedicationRequest.prototype.getId = getId;
FhirMedicationRequest.prototype.add_MainCond_and_EffectList = add_MainCond_and_EffectList;
FhirMedicationRequest.prototype.validateCBSchema = validateCBSchema;

class FhirServiceRequest {
  /**
   * @param {string} url FHIR entry element unique ULR
   * @param {string} fhirId FHIR resource identifier
   * @param {object} recObject TMR recomendation object
   * @param {string} patient patient id
   */
  constructor(
    url,
    fhirId,
    {
      id,
      text,
      motivation,
      derivedFrom,
      suggestion,
      careActionType,
      causationBeliefs,
    },
    patient
  ) {
    this._conditionList = [];
    this._forecastEffectList = [];

    this._fullUrl = url;
    this._id = fhirId;
    this._cigUri = String(derivedFrom);
    this._patient = patient;
    this._medication = servReq_ID + "/" + String(careActionType.id).slice(26);
    this._doNotRecommend = String(suggestion) === "nonrecommend";
    //create list of Conditions and ForecastEffects -both have the same number of resources
    for (let index = 0; index < causationBeliefs.length; index++) {
      const causationBelief = causationBeliefs[index];
      this.validateCBSchema(causationBelief);
      this.add_MainCond_and_EffectList(causationBelief);
    }
    
  }

  /**
   *
   */
  toJSON() {
    return {
      fullUrl: this._fullUrl,
      resource: {
        resourceType: servReq_ID,
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
              display: this._serviceDisplay,
            },
          ],
        },
        subject: {
          reference: this._patient,
        },
      },
    };
  }
}

FhirServiceRequest.prototype.getSituations = getSituations;
FhirServiceRequest.prototype.getId = getId;
FhirServiceRequest.prototype.add_MainCond_and_EffectList = add_MainCond_and_EffectList;
FhirServiceRequest.prototype.toString = toString;
FhirServiceRequest.prototype.validateCBSchema = validateCBSchema;

class FhirDetectedIssue {

  constructor(interactionType, indexInList, normsList) {
    this._fullUrl = uriPrefix + detecIs_ID + '/' + interactionType + indexInList;
    this._id = interactionType + indexInList;
    this._implicatedList = this.createImplicatedList(normsList);

    let {interaction, mitigation} = interactionCodes.get(interactionType);
    this._codingList = interaction;
    this._mitigationList = mitigation;
  }

  createImplicatedList(list) {
    //only medicationRequests have detected interactions
    if (!Array.isArray(list)) throw new Error('list of Norms in interaction is not an array');

    let  resultList = list.map(
        (item) => ( { reference: medReq_ID + '/' + String(item).slice(26)} )
      );
    return resultList;
  }


  /**
   * 
   */
  toJSON() {
     return {
      fullUrl: this._fullUrl,
      resource: {
        resourceType: detecIs_ID,
        id: this._id,
        status: "preliminary",
        code: {
          coding: this._codingList 
         }
       },
        implicated: this._implicatedList,
        mitigation: this._mitigationList
      } ;
    }
}


class FhirCarePlan {
  
    constructor(planIndex, title, patient, requestUrlList, fhirEntries){
        this._fullUrl = uriPrefix + carePlan_ID + '/' + carePlan_ID + planIndex;
        this._id = carePlan_ID + planIndex;
        this._title = title;
        this._patient = patient;
        this._activityList = addRequestRef(fhirEntries, requestUrlList);
    }

    /**
     * Conver TMR URIs into FHIR URLs
     * @param {Array} entryList list of FHIR entries already added to the instance of the response schema
     * @param {Array} urlList list of TMR recommendations as given by the resolution engine
     * 
     * @returns {string} resource unique URL
     */
    addRequestRef(entryList, urlList){
        let resultArr;
        if(Array.isArray(entryList) && Array.isArray(urlList)) {
           

            resultArr = urlList.map( 
                (urlRef) => {

                    let id = String(urlRef).slice(26);
                    //find object with same id
                    let resource = entryList.find( 
                        (item) => item.resource.id == id
                        );

                    let identifier = resource.resourceType + id;
                    //return fullURl
                    return { ref: identifier };
                }
            );
        } else {
            //throw error
        }
        return resultArr;
    }
  
  toJSON() {
    return {
      fullUrl: this._fullUrl,
      resource: {
        resourceType: carePlan_ID,
        id: this._id,
        status: "active",
        intent: "plan",
        title: this._title,
      },
      subject: {
        reference: this._patient,
      },
      activity: this._activityList,
    };
  }
}

//////////////////////////////////
/////////////////////////////////////
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
   * @param {string} code
   */
  showFullUrl(code) {
    return uriPrefix + med_ID + '/' + code;
  }

  /**
   * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
   */
  add() {
    //obtain argument from the calling function
    return ({
      id,
      text,
      motivation,
      derivedFrom,
      suggestion,
      careActionType,
      causationBeliefs,
    }) => {
      if (careActionType) {
        //create Fhir URL
        const url = this.showFullUrl(careActionType.code);
        //if it does not find key, add new medication using the care action type
        if (!this.map.has(url)) {
          this.map.set(url, new FhirMedication(url, careActionType));
        }
      }
      return this;
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

  showFullUrl(code) {
    return uriPrefix + cond_ID + '/' + code;
  }

  addOneResource(  situationTypes, patient ){

      //if not undefined, act on it
      //if(!Array.isArray(situationTypes)) throw new Error('situationTypes is not an array');

        let preSit = this.getSituations(situationTypes).preSituation;
        //console.log(JSON.stringify(preSit));
        //create Fhir URL
        const url = this.showFullUrl(preSit.value.code);

        //if it doesnt find key, add new Condition using the care action type
        if (!this.map.has(url)) {
          this.map.set(url, new FhirCondition(url, preSit, patient));
        }
      
  }

  add() {
    //obtain argument from the calling function
    return (
     causationBeliefs,
      patient
    ) => {
      //if(!Array.isArray(causationBeliefs)) throw new Error('causationBeliefs is not an array');

      //for each CB, add a condition
      for(let i =0; i<causationBeliefs.length; i++){
        
        let situationTypes = causationBeliefs[i].situationTypes;
        this.addOneResource(situationTypes, patient);
      }

      return this;
    };
  }
}

//add getSituations to prototype of ConditionResources
ConditionResources.prototype.getSituations = getSituations;
ConditionResources.prototype.getResourceList = getResourceList;

class ForecastEffectResources {
  /**
   *
   * @param {Map<string, FhirForecastEffect>} map Create a Map of  resources
   */
  constructor(map) {
    //Map containing medication resources
    this.map = map;
  }

  showFullUrl(fhirId) {
    return uriPrefix + effect_ID + '/' + fhirId;
  }

  addOneResource(
    recId,
    request,
    causationBelief,
    index,
     patient
     ){
      
      //validate CB
      //this.validateCBSchema(causationBelief);

      const { id, contribution, probability, evidence, author, transition } = causationBelief;
    const { preSituation, postSituation } = this.getSituations( transition.situationTypes);
      const fhirId = this.getId(
        preSituation,
        postSituation,
        index,
        contribution
      );
      //create Fhir URL
      const url = this.showFullUrl(fhirId);

      //if it doesnt find key, add new ForecastEffect using the care action type
      if (!this.map.has(url)) {
        this.map.set(
          url,
          new FhirForecastEffect(
            recId,
            url,
            fhirId,
            request,
            {
              id,
              contribution,
              probability,
              evidence,
              author,
              transition,
              preSituation,
              postSituation,
            },
            index,
            patient
          )
        );
      } else {
        //if it already exits, add the MedicationRequest url to the instance
        this.map.get(url).addRequestUrl(recId, request);
      }
    
  }

  /**
   * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
   */
  add() {
    //obtain argument from the calling function
    return (
      {
      id,
      text,
      motivation,
      derivedFrom,
      suggestion,
      careActionType,
      causationBeliefs,
      },
      patient
    ) => {
      //validate CBs
      if(!Array.isArray(causationBeliefs)) throw new Error(`causationBeliefs is not an array in recommendation with id ${id}.`);

      let requestType = careActionType.requestType;

      //loop over CBs
      for(let i=0; i<causationBeliefs.length; i++) {
        const cb = causationBeliefs[i];
        this.validateCBSchema(cb);
        this.addOneResource(id,requestType, cb,i, patient);
      }

      return this;
    };
  }
}
//adding function getSituations to class FhirForecastEffect
ForecastEffectResources.prototype.getSituations = getSituations;
ForecastEffectResources.prototype.getResourceList = getResourceList;
ForecastEffectResources.prototype.getId = getId;

/**
 * class to construct the set of FHIR MedicationRequests taken from the TMR data. 
 * It is also the first class to be built as it has all the required checks to validate the TMR schema.
 */
class MedicationRequestResources {
  /**
   *
   * @param {Map<string, FhirMedicationRequest>} map Create a Map of  resources
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
    return uriPrefix + medReq_ID + '/' + fhirId;
  }

  /**
   * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
   */
  add() {
    //obtain argument from the calling function
    return (
      patient,
      interactions,
      recommendations
    ) => {

      //validate interactions schema
      this.validateInteractionsSchema(interactions);

      //apply function to each recommendation
      for (let i = 0; i < recommendations.length; i++) {
        
        const rec  = recommendations[i];

           //validate recommendations. It validates both medication and services
        this.validateRecschema(rec);

        let request = rec.careActionType.requestType;
        //check is a medication request
       if (medReq_ID === requestMap.get(request)) {

        const fhirId = String(id).slice(26);

        //create Fhir URL
        const url = this.showFullUrl(fhirId);
        //if it doesnot find key, add new medication using the care action type
        if (!this.map.has(url)) {
          this.map.set(
            url,
            new FhirMedicationRequest(
              url,
              fhirId,
              {
                id,
                text,
                motivation,
                derivedFrom,
                suggestion,
                careActionType,
                causationBeliefs,
              },
              interactions,
              patient
            )
          );
        }

      } //else skip to the next rec

      }//end of for loop

      //return the object
      return this;
    };
  }

  validateInteractionsSchema(interactions){
     //check interactions  is array
    if(!Array.isArray(interactions)) throw new Error('interactions is not an array');

    const interPropList = ['type', 'interactionNorms'], normPropList = ['type', 'recId'];

    for (let index = 0; index < interactions.length; index++) {
      const normObj = interactions[index];
      if( interPropList.every( prop => prop in normObj)) throw new Error(`interaction object is missing property ${prop} at index ${index}.`);
      for (let i = 0; i < normObj.length; i++) {
        const norm = normObj[i];
        if( normPropList.every( prop => prop in norm)) throw new Error(`interactionNorms is missing property ${prop} at index ${i}.`);
      }
    }
    
  }

  /**
   * Validate one recommendation
   * @param {object} recommendation 
   */
  validateRecschema(recommendation){

      //check recommendation is arrays
      //if(!Array.isArray(recommendations)) throw new Error('recommendations is not an array');
      //check arrays have expected properties
      const recPropList = [
        'id',
       // 'text',
       // 'motivation',
        'derivedFrom',
        'suggestion',
        'careActionType',
        'causationBeliefs'
      ], carePropList = ['id', 'code', 'requestType', 'display'];

      if(!recPropList.every( prop => prop in recommendations)) throw new Error(`property '${prop}' is missing in recommendations array.`);

      //NExt, check careActionType, causationBeliefs  are arrays
      
      //finally, check for properties on the 3 arrays from above
      if(!carePropList.every( prop => prop in careActionType)) throw new Error('one or more expected properties are missing in careActionType array.');
      if(!Array.isArray(careActionType)) throw new Error('interactions is not an array');
      if(!Array.isArray(causationBeliefs)) throw new Error('recommendations is not an array');
      //CB checking is done elsewhere

  }
}

//adding function getSituations to class FhirForecastEffect
MedicationRequestResources.prototype.getSituations = getSituations;
MedicationRequestResources.prototype.getResourceList = getResourceList;
MedicationRequestResources.prototype.getId = getId;

class ServiceRequestResources {
  /**
   *
   * @param {Map<string, FhirServiceRequest>} map Create a Map of  resources
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
    return uriPrefix + servReq_ID + fhirId;
  }

  /**
   * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
   */
  add() {
    //obtain argument from the calling function
    return (
      {
        id,
        text,
        motivation,
        derivedFrom,
        suggestion,
        careActionType,
        causationBeliefs,
      },
      patient
    ) => {
      if (servReq_ID === requestMap.get(careActionType.requestType)) {
        const fhirId = String(id).slice(26);
        //create Fhir URL
        const url = this.showFullUrl(fhirId);
        //if it does find key, add new medication using the care action type
        if (!this.map.has(url)) {
          this.map.set(
            url,
            new FhirServiceRequest(
              url,
              fhirId,
              {
                id,
                text,
                motivation,
                derivedFrom,
                suggestion,
                careActionType,
                causationBeliefs,
              },
              patient
            )
          );
        }
      }
      return this;
    };
  }
}

//adding function getSituations to class FhirForecastEffect
ServiceRequestResources.prototype.getSituations = getSituations;
ServiceRequestResources.prototype.getResourceList = getResourceList;
ServiceRequestResources.prototype.getId = getId;


class DetectedIssueResources {

    /**
     *
     * 
     */
    constructor() {
      //Map containing  FhirDetectedIssue resources
      this._issueArr = [];
    }
  
    /**
     * Given a TMR Recommendation, converts a tmr interaction into a FHIR DetectedIssue and adds it to a Map object, if the interaction has not been converted before.
     */
    add() {
      //obtain argument from the calling function
      return (
        arrayInter
      ) => {
        if(!Array.isArray(arrayInter) )  throw new Error('interactions object is not an array');

        let interaction;
          for (let index = 0; index < arrayInter.length; index++) {
            interaction = arrayInter[index];
            if(!(interaction.hasOwnProperty('type') && interaction.hasOwnProperty('interactionNorms')) ) throw new Error('missing type or interactionNorms properties on interaction array');
            this._issueArr.push(new FhirDetectedIssue(interaction.type, index, interaction.interactionNorms));
          }
        return this;
      };
    }
  }

  class CarePlanResources {

     constructor(){
         this._carePlanArr = [];
     }
     add() {
         return ( 
             extensions,
             fhirReqEntries,
             title,
             patient,
             ) => {

                 if(Array.isArray(extensions) && Array.isArray(fhirReqEntries)){

                   for (let index = 0; index < extensions.length; index++) {

                     const extenList = extensions[index];
                     let requestUrlList = extenList.map( extension => extension.aboutRecommendation.id );
                     this._carePlanArr.push(
                         new FhirCarePlan(index, title, patient, requestUrlList ,fhirReqEntries)
                     );

                   }
                 } else {
                    throw new Error('One of the parameters is not an array as expected in class CarePlanResource');
                 }

             return this;
             };
        }
  }



//use case
let req ={
  "EHR": {
      "selectedTreatment": {
          "resource": {
              "reference": {
                  "resourceType": "Observation",
                  "id": "COPD.group",
                  "text": "COPD GOLD group"
              },
              "result": {
                  "code": "B",
                  "display": "COPD GOLD group B"
              },
              "other": {
                  "drugTypePreferences": {
                      "reference": {
                          "refId": "COPD.group",
                          "resultCode": "B"
                      },
                      "entries": [
                           {
                                  "preferred": {
                                      "administrationOf": "Laba"
                                  },
                                  "alternative": [
                                      {
                                          "administrationOf": "LabaLama"
                                      }
                                  ]
                              },
                              {
                                  "preferred": {
                                      "administrationOf": "Lama"
                                  },
                                  "alternative": [
                                      {
                                          "administrationOf": "LabaLama"
                                      }
                                  ]
                              }
                      ]
                  }
              }
          }
      }
  },
  "DSS": {
      "proposedTreatment": {
          "resource": {
              "reference": {
                  "resourceType": "Observation",
                  "id": "COPD.group",
                  "text": "COPD GOLD group"
              },
              "result": {
                  "code": "B",
                  "display": "COPD GOLD group B"
              },
              "other": {
                  "drugTypes": {
                      "drugTypePreferences": {
                          "reference": {
                              "refId": "COPD.group",
                              "resultCode": "B"
                          },
                          "entries": [
                              {
                                  "preferred": {
                                      "administrationOf": "Laba"
                                  },
                                  "alternative": [
                                      {
                                          "administrationOf": "LabaLama"
                                      }
                                  ]
                              },
                              {
                                  "preferred": {
                                      "administrationOf": "Lama"
                                  },
                                  "alternative": [
                                      {
                                          "administrationOf": "LabaLama"
                                      }
                                  ]
                              }
                          ]
                      }
                  }
              }
          }
      }
  },
  "TMR": {
      "guidelineGroup": {
          "id": "CIG-2803202017350345",
          "interactions": [
              {
                  "type": "alternative",
                  "interactionNorms": [
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "type": "primary"
                      },
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                          "type": "primary"
                      },
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                          "type": "primary"
                      }
                  ]
              },
              {
                  "type": "repetition",
                  "interactionNorms": [
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "type": "primary"
                      },
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                          "type": "primary"
                      },
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                          "type": "primary"
                      }
                  ]
              },
              {
                  "type": "contradiction",
                  "interactionNorms": [
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "type": "primary"
                      },
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
                          "type": "primary"
                      }
                  ]
              },
              {
                  "type": "contradiction",
                  "interactionNorms": [
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                          "type": "primary"
                      },
                      {
                          "recId": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
                          "type": "primary"
                      }
                  ]
              }
          ],
          "recommendations": [
              {
                  "id": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
                  "text": "clinician should avoid recommending administration of Beta Agonist bronchodilators to patients with cardiovascular disease",
                  "motivation": "none",
                  "suggestion": "nonrecommend",
                  "careActionType": {
                      "id": "http://anonymous.org/data/DrugCatBetaAgonist",
                      "code": "BetaAgonist",
                      "display": "administration of Beta Agonist bronchodilator",
                      "requestType": 0
                  },
                  "causationBeliefs": [
                      {
                          "id": "http://anonymous.org/data/CBBetaAgonistIncLowRiskCdr",
                          "contribution": "negative",
                          "probability": "always",
                          "evidence": "high-level",
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
                                      "id": "http://anonymous.org/data/SitHighRiskCrd",
                                      "type": "hasExpectedSituation",
                                      "value": {
                                          "code": "SitHighRiskCrd",
                                          "display": "a high risk of having cardiac rhythm disturbances"
                                      }
                                  }
                              ]
                          }
                      },
                      {
                        "id": "http://anonymous.org/data/CBLbLmaDecModAls",
                        "contribution": "negative",
                        "probability": "always",
                        "evidence": "high-level",
                        "author": "JDA",
                        "transition": {
                            "id": "http://anonymous.org/data/TrDecModAls",
                            "effect": "decrease",
                            "property": {
                                "id": "http://anonymous.org/data/PropAls",
                                "display": "airflow limitation severity",
                                "code": "Als"
                            },
                            "situationTypes": [
                                {
                                    "id": "http://anonymous.org/data/SitModAls",
                                    "type": "hasTransformableSituation",
                                    "value": {
                                        "code": "SitModAls",
                                        "display": "a moderate airflow limitation severity"
                                    }
                                },
                                {
                                    "id": "http://anonymous.org/data/SitMildAls",
                                    "type": "hasExpectedSituation",
                                    "value": {
                                        "code": "SitMildAls",
                                        "display": "a mild airflow limitation severity"
                                    }
                                }
                            ]
                        }
                    }
                  ]
              },
              {
                  "id": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                  "text": "Clinician should recommend administering LABA bronchodilator",
                  "motivation": "none",
                  "suggestion": "recommend",
                  "careActionType": {
                      "id": "http://anonymous.org/data/DrugTLaba",
                      "code": "Laba",
                      "display": "administration of LABA bronchodilator",
                      "requestType": 0
                  },
                  "causationBeliefs": [
                      {
                          "id": "http://anonymous.org/data/CBLabaDecModAls",
                          "contribution": "positive",
                          "probability": "always",
                          "evidence": "high-level",
                          "author": "Jesus",
                          "transition": {
                              "id": "http://anonymous.org/data/TrDecModAls",
                              "effect": "decrease",
                              "property": {
                                  "id": "http://anonymous.org/data/PropAls",
                                  "display": "airflow limitation severity",
                                  "code": "Als"
                              },
                              "situationTypes": [
                                  {
                                      "id": "http://anonymous.org/data/SitModAls",
                                      "type": "hasTransformableSituation",
                                      "value": {
                                          "code": "SitModAls",
                                          "display": "a moderate airflow limitation severity"
                                      }
                                  },
                                  {
                                      "id": "http://anonymous.org/data/SitMildAls",
                                      "type": "hasExpectedSituation",
                                      "value": {
                                          "code": "SitMildAls",
                                          "display": "a mild airflow limitation severity"
                                      }
                                  }
                              ]
                          }
                      }
                  ]
              },
              {
                  "id": "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                  "text": "Clinician should recommend administering LAMA bronchodilator",
                  "motivation": "none",
                  "suggestion": "recommend",
                  "careActionType": {
                      "id": "http://anonymous.org/data/DrugTLama",
                      "code": "Lama",
                      "display": "administration of LAMA bronchodilator",
                      "requestType": 0
                  },
                  "causationBeliefs": [
                      {
                          "id": "http://anonymous.org/data/CBLamaDecModAls",
                          "contribution": "positive",
                          "probability": "always",
                          "evidence": "high-level",
                          "author": "JDA",
                          "transition": {
                              "id": "http://anonymous.org/data/TrDecModAls",
                              "effect": "decrease",
                              "property": {
                                "id": "http://anonymous.org/data/PropAls",
                                  "display": "airflow limitation severity",
                                  "code": "Als"
                              },
                              "situationTypes": [
                                  {
                                      "id": "http://anonymous.org/data/SitModAls",
                                      "type": "hasTransformableSituation",
                                      "value": {
                                          "code": "SitModAls",
                                          "display": "a moderate airflow limitation severity"
                                      }
                                  },
                                  {
                                      "id": "http://anonymous.org/data/SitMildAls",
                                      "type": "hasExpectedSituation",
                                      "value": {
                                          "code": "SitMildAls",
                                          "display": "a mild airflow limitation severity"
                                      }
                                  }
                              ]
                          }
                      }
                  ]
              },
              {
                  "id": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                  "text": "Clinician should recommend administering LABA+LAMA bronchodilator",
                  "motivation": "none",
                  "suggestion": "recommend",
                  "careActionType": {
                      "id": "http://anonymous.org/data/DrugTLabaLama",
                      "code": "LabaLama",
                      "display": "administration of LABA+LAMA bronchodilator",
                      "requestType": 0
                  },
                  "causationBeliefs": [
                      {
                          "id": "http://anonymous.org/data/CBLabaLamaDecModAls",
                          "contribution": "positive",
                          "probability": "always",
                          "evidence": "high-level",
                          "author": "JDA",
                          "transition": {
                              "id": "http://anonymous.org/data/TrDecModAls",
                              "effect": "decrease",
                              "property": {
                                  "display": "airflow limitation severity",
                                  "code": "Als",
                                  "id": "http://anonymous.org/data/PropAls"
                              },
                              "situationTypes": [
                                  {
                                      "id": "http://anonymous.org/data/SitModAls",
                                      "type": "hasTransformableSituation",
                                      "value": {
                                          "code": "SitModAls",
                                          "display": "a moderate airflow limitation severity"
                                      }
                                  },
                                  {
                                      "id": "http://anonymous.org/data/SitMildAls",
                                      "type": "hasExpectedSituation",
                                      "value": {
                                          "code": "SitMildAls",
                                          "display": "a mild airflow limitation severity"
                                      }
                                  }
                              ]
                          }
                      }
                  ]
              }
          ]
      }
  }

};


let res = {
  "extensions": [
      {
          "extension": [
              {
                  "aboutRecommendation": {
                      "id": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                      "text": "Clinician should recommend administering LABA+LAMA bronchodilator",
                      "causationBeliefs": [
                          {
                              "id": "http://anonymous.org/data/CBLabaLamaDecModAls",
                              "contribution": "positive",
                              "transition": {
                                  "id": "http://anonymous.org/data/TrDecModAls",
                                  "effect": "decrease",
                                  "property": {
                                      "display": "airflow limitation severity",
                                      "code": "Als"
                                  },
                                  "situationTypes": [
                                      {
                                          "id": "http://anonymous.org/data/SitModAls",
                                          "type": "hasTransformableSituation",
                                          "value": {
                                              "code": "SitModAls",
                                              "display": "a moderate airflow limitation severity"
                                          }
                                      },
                                      {
                                          "id": "http://anonymous.org/data/SitMildAls",
                                          "type": "hasExpectedSituation",
                                          "value": {
                                              "code": "SitMildAls",
                                              "display": "a mild airflow limitation severity"
                                          }
                                      }
                                  ]
                              }
                          }
                      ],
                      "reasonsComponents": [
                          {
                              "careAction": "LabaLama",
                              "contribution": "positive",
                              "contributionON": "Als",
                              "contributionTO": "decrease",
                              "from": "SitModAls",
                              "to": "SitMildAls"
                          }
                      ],
                      "reasons": "administration of LABA+LAMA bronchodilator _HAS_ positive contribution _ON_ airflow limitation severity _TO_ decrease _FROM_ a moderate airflow limitation severity _TO_ a mild airflow limitation severity"
                  },
                  "interactionsInformation": {
                      "interactingRecommendations": [
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
                              "interactionTypes": [
                                  "contradiction"
                              ],
                              "preferred": 0
                          },
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                              "interactionTypes": [
                                  "alternative",
                                  "repetition"
                              ],
                              "preferred": 0
                          },
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                              "interactionTypes": [
                                  "alternative",
                                  "repetition"
                              ],
                              "preferred": 0
                          }
                      ],
                      "text": "Considered alternatives: recommend administration of LABA bronchodilator; recommend administration of LAMA bronchodilator. Considered contradictory recommendations: nonrecommend administration of Beta Agonist bronchodilator. Considered repetitive recommendations: recommend administration of LABA bronchodilator; recommend administration of LAMA bronchodilator. No recommendations in repairable relation considered.",
                      "alternatives": [
                          "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould"
                      ],
                      "contradictions": [
                          "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot"
                      ],
                      "repetitions": [
                          "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould"
                      ],
                      "repairables": []
                  }
              }
          ]
      },
      {
          "extension": [
              {
                  "aboutRecommendation": {
                      "id": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                      "text": "Clinician should recommend administering LABA bronchodilator",
                      "causationBeliefs": [
                          {
                              "id": "http://anonymous.org/data/CBLabaDecModAls",
                              "contribution": "positive",
                              "transition": {
                                  "id": "http://anonymous.org/data/TrDecModAls",
                                  "effect": "decrease",
                                  "property": {
                                      "display": "airflow limitation severity",
                                      "code": "Als"
                                  },
                                  "situationTypes": [
                                      {
                                          "id": "http://anonymous.org/data/SitModAls",
                                          "type": "hasTransformableSituation",
                                          "value": {
                                              "code": "SitModAls",
                                              "display": "a moderate airflow limitation severity"
                                          }
                                      },
                                      {
                                          "id": "http://anonymous.org/data/SitMildAls",
                                          "type": "hasExpectedSituation",
                                          "value": {
                                              "code": "SitMildAls",
                                              "display": "a mild airflow limitation severity"
                                          }
                                      }
                                  ]
                              }
                          }
                      ],
                      "reasonsComponents": [
                          {
                              "careAction": "Laba",
                              "contribution": "positive",
                              "contributionON": "Als",
                              "contributionTO": "decrease",
                              "from": "SitModAls",
                              "to": "SitMildAls"
                          }
                      ],
                      "reasons": "administration of LABA bronchodilator _HAS_ positive contribution _ON_ airflow limitation severity _TO_ decrease _FROM_ a moderate airflow limitation severity _TO_ a mild airflow limitation severity"
                  },
                  "interactionsInformation": {
                      "interactingRecommendations": [
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
                              "interactionTypes": [
                                  "contradiction"
                              ],
                              "preferred": 0
                          },
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                              "interactionTypes": [
                                  "alternative",
                                  "repetition"
                              ],
                              "preferred": 0
                          },
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                              "interactionTypes": [
                                  "alternative",
                                  "repetition"
                              ],
                              "preferred": 0
                          }
                      ],
                      "text": "Considered alternatives: recommend administration of LAMA bronchodilator; recommend administration of LABA+LAMA bronchodilator. Considered contradictory recommendations: nonrecommend administration of Beta Agonist bronchodilator. Considered repetitive recommendations: recommend administration of LAMA bronchodilator; recommend administration of LABA+LAMA bronchodilator. No recommendations in repairable relation considered.",
                      "alternatives": [
                          "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould"
                      ],
                      "contradictions": [
                          "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot"
                      ],
                      "repetitions": [
                          "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould"
                      ],
                      "repairables": []
                  }
              }
          ]
      },
      {
          "extension": [
              {
                  "aboutRecommendation": {
                      "id": "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
                      "text": "clinician should avoid recommending administration of Beta Agonist bronchodilators to patients with cardiovascular disease",
                      "causationBeliefs": [
                          {
                              "id": "http://anonymous.org/data/CBBetaAgonistIncLowRiskCdr",
                              "contribution": "negative",
                              "transition": {
                                  "id": "http://anonymous.org/data/TrIncLowRiskCdr",
                                  "effect": "increase",
                                  "property": {
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
                      ],
                      "reasonsComponents": [
                          {
                              "careAction": "BetaAgonist",
                              "contribution": "negative",
                              "contributionON": "Crd",
                              "contributionTO": "increase",
                              "from": "SitLowRiskCrd",
                              "to": "SitHighRiskCrd"
                          }
                      ],
                      "reasons": "administration of Beta Agonist bronchodilator _HAS_ negative contribution _ON_ risk of having cardiac rhythm disturbances _TO_ increase _FROM_ a low risk of having cardiac rhythm disturbances _TO_ a high risk of having cardiac rhythm disturbances"
                  },
                  "interactionsInformation": {
                      "interactingRecommendations": [
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                              "interactionTypes": [
                                  "contradiction"
                              ],
                              "preferred": 0
                          },
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                              "interactionTypes": [
                                  "contradiction"
                              ],
                              "preferred": 0
                          }
                      ],
                      "text": "No applicable alternatives considered. Considered contradictory recommendations: recommend administration of LABA bronchodilator; recommend administration of LABA+LAMA bronchodilator. No repetitive recommendations considered. No recommendations in repairable relation considered.",
                      "alternatives": [],
                      "contradictions": [
                          "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould"
                      ],
                      "repetitions": [],
                      "repairables": []
                  }
              },
              {
                  "aboutRecommendation": {
                      "id": "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
                      "text": "Clinician should recommend administering LAMA bronchodilator",
                      "causationBeliefs": [
                          {
                              "id": "http://anonymous.org/data/CBLamaDecModAls",
                              "contribution": "positive",
                              "transition": {
                                  "id": "http://anonymous.org/data/TrDecModAls",
                                  "effect": "decrease",
                                  "property": {
                                      "display": "airflow limitation severity",
                                      "code": "Als"
                                  },
                                  "situationTypes": [
                                      {
                                          "id": "http://anonymous.org/data/SitModAls",
                                          "type": "hasTransformableSituation",
                                          "value": {
                                              "code": "SitModAls",
                                              "display": "a moderate airflow limitation severity"
                                          }
                                      },
                                      {
                                          "id": "http://anonymous.org/data/SitMildAls",
                                          "type": "hasExpectedSituation",
                                          "value": {
                                              "code": "SitMildAls",
                                              "display": "a mild airflow limitation severity"
                                          }
                                      }
                                  ]
                              }
                          }
                      ],
                      "reasonsComponents": [
                          {
                              "careAction": "Lama",
                              "contribution": "positive",
                              "contributionON": "Als",
                              "contributionTO": "decrease",
                              "from": "SitModAls",
                              "to": "SitMildAls"
                          }
                      ],
                      "reasons": "administration of LAMA bronchodilator _HAS_ positive contribution _ON_ airflow limitation severity _TO_ decrease _FROM_ a moderate airflow limitation severity _TO_ a mild airflow limitation severity"
                  },
                  "interactionsInformation": {
                      "interactingRecommendations": [
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                              "interactionTypes": [
                                  "alternative",
                                  "repetition"
                              ],
                              "preferred": 0
                          },
                          {
                              "interactingRecommendationId": "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
                              "interactionTypes": [
                                  "alternative",
                                  "repetition"
                              ],
                              "preferred": 0
                          }
                      ],
                      "text": "Considered alternatives: recommend administration of LABA bronchodilator; recommend administration of LABA+LAMA bronchodilator. No contradicting recommendations considered. Considered repetitive recommendations: recommend administration of LABA bronchodilator; recommend administration of LABA+LAMA bronchodilator. No recommendations in repairable relation considered.",
                      "alternatives": [
                          "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould"
                      ],
                      "contradictions": [],
                      "repetitions": [
                          "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
                          "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould"
                      ],
                      "repairables": []
                  }
              }
          ]
      }
  ]
};


let condObj = new ConditionResources(new Map());
let medReqObj = new MedicationRequestResources(new Map());
let servReq = new ServiceRequestResources(new Map());
let effectObj = new ForecastEffectResources(new Map());
let medObj = new MedicationResources(new Map());
let issueObj = new DetectedIssueResources();
let planObj = new CarePlanResources()

//exports.cardParams =  cardParams;
//exports.createCard = (options) => new Card(options);

exports.args = req;
exports.mitigation = res;
exports.addMedication = medObj.add();
exports.addCondition = condObj.add();
exports.addMedicationRequest = medReqObj.add();
exports.addServiceRequest = servReq.add();
exports.addForecastEffect = effectObj.add();
exports.addDetectedIssues = issueObj.add();
exports.addCarePlans = planObj.add();
