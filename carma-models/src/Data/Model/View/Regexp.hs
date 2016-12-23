{-# LANGUAGE OverloadedStrings #-}

module Data.Model.View.Regexp

where

import Data.Text

regexpDate :: Text
regexpDate = "^\\d{2}\\.\\d{2}\\.\\d{4}$"

regexpDateTime :: Text
regexpDateTime = "^\\d{1,2}\\.\\d{1,2}\\.\\d{4} \\d{1,2}:\\d{2}:\\d{2}$"

regexpDouble :: Text
regexpDouble = "^[0-9]+([.,][0-9]+)?$"

regexpEmail :: Text
regexpEmail = "^[\\w\\+\\.\\-]+@[\\w\\+\\.\\-]+\\.\\w+$"

regexpNumber :: Text
regexpNumber = "^[0-9]+$"

regexpPhone :: Text
regexpPhone = "^\\+7\\d{10}$"

regexpPlateNum :: Text
regexpPlateNum = "^([АВЕКМНОРСТУХавекмнорстух]\\d{3}[АВЕКМНОРСТУХавекмнорстух]{2}\\d{2,3})|-$"

regexpTextSender :: Text
regexpTextSender = "^[0-9a-zA-Z_\\-&\\.]+$"

regexpTimespan :: Text
regexpTimespan = "(^\\d+:[012345][0-9]$)|^$"

regexpVIN :: Text
regexpVIN = "^[0-9a-hA-Hj-nJ-NpPr-zR-Z]{17}$"

regexpYear :: Text
regexpYear = "^[12][09][0-9]{2}$"
