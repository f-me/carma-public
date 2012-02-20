function metaPages() {
  return {
    "call": {
      left: ["callInfo"]
    },
    "case": {
      left: ["caseInfo", "services", "addService"],
      right: ["programInfo","caseHistory"],
      warnings: ["requiredFields"]
    }
  };
}

function metaForms() {
  return {
    callInfo: {
      fields:
        [{wazzup: {type:"textarea",label:"Что случилось"}}
        ]
    }
  };
}
