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
const interactionCodes = new Map(
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
);

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

function add_MainCond_and_EffectList(causationBeliefs) {
  let condition, forecastEffect;

  if (Array.isArray(causationBeliefs)) {
    causationBeliefs.map(function (
      { id, contribution, probability, evidence, author, transition },
      index
    ) {
      //extract pre and post situation objects
      if (Array.isArray(transition.situationTypes)) {
        let { preSituation, postSituation } = this.getSituations(
          transition.situationTypes
        );
        condition = cond_ID + "/" + preSituation.value.code;
        //add condition id if this is the main effect (side effects conditions can be fetched via forecast effects)
        if (index === 0) this._conditionList.push({ reference: condition });

        forecastEffect =
          effect_ID +
          "/" +
          this.getId(preSituation, postSituation, index, contribution);
        //add forecastEffect id
        this._forecastEffectList.push({ reference: forecastEffect });
      } else {
        throw new Error('causatinBeliefs is not an array in funct add_MainCond_and_EffectList');
      }
    },
    this);
  } else {
    throw new Error('transition.situationTypes is not an array in funct add_MainCond_and_EffectList');
  }
}

function toString() {
  return JSON.stringify(this.toJSON());
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
    this.add_MainCond_and_EffectList(causationBeliefs);
    this.addDetectedIssues(interactions);
  }

  /**
   *
   * @param {Array} interactionList           TODO: check algorithm
   */
  addDetectedIssues(interactionList) {
    if (Array.isArray(interactionList)) {
      //reconstruct TMR URI for recommendation
      const tmrId = uriPrefix + this._id;
      let refId;

      interactionList.map(({ type, interactionNorms }, index) => {
        //console.log('inside first map fun');
        refId = detecIs_ID + "/" + type + (index + 1);
        //console.log('ref id is ' + refId);
        if (Array.isArray(interactionNorms)) {
          //console.log('interactionNorms is array');
          //add each norm w/interaction to the detectedIssue List
          interactionNorms.map((norm) => {
            if (tmrId === String(norm.recId)) {
              // console.log('inside conditional where norm recId i s ' + String(norm.recId));
              this._detectedIssueList.push({
                reference: refId,
              });
            } else {
              //console.log(norm.recId + ' not equal to ' + tmrId + '\n');
            }
          }, this);
        } else {
          //TODO: throw error
          //console.log('not array');
        }
      }, this);
    }
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
    this.add_MainCond_and_EffectList(causationBeliefs);
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

class FhirDetectedIssue {

  constructor(interactionType, interactionListIndex, normsList) {
    this._fullUrl = uriPrefix + detecIs_ID + interactionType + interactionListIndex;
    this._id = interactionType + interactionListIndex;
    this._implicatedList = createImplicatedList(normsList);

    let {interaction, mitigation} = interactionCodes.get(interactionType);
    this._codingList = interaction;
    this._mitigationList = mitigation;
  }

  createImplicatedList(list) {
    let resultList = [];

    //only medicationRequests have detected interactions
    if (Array.isArray(list)) {
      resultList = list.map(
        (item) => ( { reference: medReq_ID + '/' + String(item).slice(26)} )
      );
    } else {
      //throw error
    }
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
          coding: this.codingList 
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
    return uriPrefix + med_ID + code;
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
    return uriPrefix + cond_ID + code;
  }

  add() {
    //obtain argument from the calling function
    return (
      { id, contribution, probability, evidence, author, transition },
      patient
    ) => {
      const situationTypes = transition.situationTypes;

      //if not undefined, act on it
      if (Array.isArray(situationTypes)) {
        let preSit = this.getSituations(situationTypes).preSituation;
        console.log(JSON.stringify(preSit));
        //create Fhir URL
        const url = this.showFullUrl(preSit.value.code);

        //if it doesnt find key, add new Condition using the care action type
        if (!this.map.has(url)) {
          this.map.set(url, new FhirCondition(url, preSit, patient));
        }
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
    return uriPrefix + effect_ID + fhirId;
  }

  /**
   * Given a TMR Recommendation, converts a tmr care action type into a FHIR medication and adds it to a Map object, if the care action has not been converted before.
   */
  add() {
    //obtain argument from the calling function
    return (
      recId,
      request,
      { id, contribution, probability, evidence, author, transition },
      index,
      patient
    ) => {
      //if not of type undefined, act on it
      if (id && contribution && probability && evidence && transition) {
        const { preSituation, postSituation } = this.getSituations(
          transition.situationTypes
        );
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
      } else {
        //TODO: error checking
      }
      return this;
    };
  }
}
//adding function getSituations to class FhirForecastEffect
ForecastEffectResources.prototype.getSituations = getSituations;
ForecastEffectResources.prototype.getResourceList = getResourceList;
ForecastEffectResources.prototype.getId = getId;

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
    return uriPrefix + medReq_ID + fhirId;
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
      interactions,
      patient
    ) => {
      //check is a medication request
      if (medReq_ID === requestMap.get(careActionType.requestType)) {
        const fhirId = String(id).slice(26);
        //create Fhir URL
        const url = this.showFullUrl(fhirId);
        //if it does find key, add new medication using the care action type
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
      }

      //return the object
      return this;
    };
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
        arrayInt
      ) => {
        if( Array.isArray(arrayInt) ) {
            this._issueArr = arrayInt.map( 
                (interaction, index) => new FhirDetectedIssue(interaction.type, index, interaction.interactionNorms)
            );
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
             title,
             patient,
             fhirReqEntries
             ) => {
                 if(Array.isArray(extensions) && Array.isArray(fhirReqEntries)){
                    this._carePlanArr = extensions.map(
                        (extenList, index) => {
                            let requestUrlList = extenList.map( extension => extension.aboutRecommendation.id );
                            return new FhirCarePlan(index, title, patient, requestUrlList ,fhirReqEntries);
                        }
                    );
                 } else {
                    throw new Error('One of the parameters is not an array as expected in class CarePlanResource');
                 }

             return this;
             };
        }
  }



//use case
const TMRobject = {
  id: "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
  text:
    "clinician should avoid recommending administration of Beta Agonist bronchodilators to patients with cardiovascular disease",
  motivation: "none",
  derivedFrom: "GOLD COPD 2017",
  suggestion: "nonrecommend",
  careActionType: {
    id: "http://anonymous.org/data/DrugCatBetaAgonist",
    code: "DrugCatBetaAgonist",
    display: "administration of Beta Agonist bronchodilator",
    requestType: 0,
    drugLabel: "BetaAgonist",
  },
  causationBeliefs: [
    {
      id: "http://anonymous.org/data/CBBetaAgonistIncLowRiskCdr",
      contribution: "negative",
      probability: "always",
      evidence: "High Level",
      author: "JDA",
      transition: {
        id: "http://anonymous.org/data/TrIncLowRiskCdr",
        effect: "increase",
        property: {
          id: "http://anonymous.org/data/PropCrd",
          display: "risk of having cardiac rhythm disturbances",
          code: "Crd",
        },
        situationTypes: [
          {
            id: "http://anonymous.org/data/SitLowRiskCrd",
            type: "hasTransformableSituation",
            value: {
              code: "SitLowRiskCrd",
              display: "a low risk of having cardiac rhythm disturbances",
            },
          },
          {
            id: "http://anonymous.org/data/SitMildAls",
            type: "hasExpectedSituation",
            value: {
              code: "SitHighRiskCrd",
              display: "a high risk of having cardiac rhythm disturbances",
            },
          },
        ],
      },
    },
    {
      id: "http://anonymous.org/data/CBBetaAgonistIncLowRiskCdr4",
      contribution: "negative",
      probability: "always",
      evidence: "High Level",
      author: "JDA",
      transition: {
        id: "http://anonymous.org/data/TrIncLowRiskCdr4",
        effect: "increase",
        property: {
          id: "http://anonymous.org/data/PropCrd4",
          display: "risk of having cardiac rhythm disturbances",
          code: "Crd",
        },
        situationTypes: [
          {
            id: "http://anonymous.org/data/SitLowRiskCrd4",
            type: "hasTransformableSituation",
            value: {
              code: "SitLowRiskCrd",
              display: "a low risk of having cardiac rhythm disturbances",
            },
          },
          {
            id: "http://anonymous.org/data/SitMildAls4",
            type: "hasExpectedSituation",
            value: {
              code: "SitHighRiskCrd",
              display: "a high risk of having cardiac rhythm disturbances",
            },
          },
        ],
      },
    },
  ],
};

const TMRobject2 = {
  id: "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
  text:
    "clinician should avoid recommending administration of Beta Agonist bronchodilators to patients with cardiovascular disease",
  motivation: "none",
  derivedFrom: "GOLD COPD 2017",
  suggestion: "recommend",
  careActionType: {
    id: "http://anonymous.org/data/DrugTLaba",
    code: "DrugTLaba",
    display: "administration of LABA",
    requestType: 1,
    drugLabel: "LABA",
  },
  causationBeliefs: [
    {
      id: "http://anonymous.org/data/CBBetaAgonistIncLowRiskCdr",
      contribution: "negative",
      probability: "always",
      evidence: "High Level",
      author: "JDA",
      transition: {
        id: "http://anonymous.org/data/TrIncLowRiskCdr",
        effect: "increase",
        property: {
          id: "http://anonymous.org/data/PropCrd",
          display: "risk of having cardiac rhythm disturbances",
          code: "Crd",
        },
        situationTypes: [
          {
            id: "http://anonymous.org/data/SitModAls",
            type: "hasTransformableSituation",
            value: {
              code: "SitModAls",
              display: "a low risk of having cardiac rhythm disturbances",
            },
          },
          {
            id: "http://anonymous.org/data/SitMildAls",
            type: "hasExpectedSituation",
            value: {
              code: "SitMildAls",
              display: "a high risk of having cardiac rhythm disturbances",
            },
          },
        ],
      },
    },
  ],
};

const interactions = [
  {
    type: "alternative",
    interactionNorms: [
      {
        recId: "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
        type: "primary",
      },
      {
        recId: "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
        type: "primary",
      },
      {
        recId: "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
        type: "primary",
      },
    ],
  },
  {
    type: "repetition",
    interactionNorms: [
      {
        recId: "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
        type: "primary",
      },
      {
        recId: "http://anonymous.org/data/RecCOPD-LamaDecModAlsShould",
        type: "primary",
      },
      {
        recId: "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
        type: "primary",
      },
    ],
  },
  {
    type: "contradiction",
    interactionNorms: [
      {
        recId: "http://anonymous.org/data/RecCOPD-LabaDecModAlsShould",
        type: "primary",
      },
      {
        recId:
          "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
        type: "primary",
      },
    ],
  },
  {
    type: "contradiction",
    interactionNorms: [
      {
        recId: "http://anonymous.org/data/RecCOPD-LabaLamaDecModAlsShould",
        type: "primary",
      },
      {
        recId:
          "http://anonymous.org/data/RecCOPD-BetaAgonistIncLowRiskCdrShouldnot",
        type: "primary",
      },
    ],
  },
];

//const medObj = new MedicationResources(new Map());
let condObj = new ConditionResources(new Map());
let medReqObj = new MedicationRequestResources(new Map());
let servReq = new ServiceRequestResources(new Map());
let effectObj = new ForecastEffectResources(new Map());

//exports.cardParams =  cardParams;
//exports.createCard = (options) => new Card(options);
//exports.addMedication = medObj.add();
exports.addCondition = condObj.add();
exports.addMedicationRequest = medReqObj.add();
exports.addServiceRequest = servReq.add();
exports.addForecastEffect = effectObj.add();
exports.rec1 = TMRobject;
exports.rec2 = TMRobject2;
exports.interactions = interactions;
