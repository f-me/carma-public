This service enables unfortunate car owners (Clients) to share their location
using Web API.

The process goes as follows:
  - Call-center operator
    - asks the Client for their mobile phone number
    - enters it into the system
    - pushes the button to request client's location
  - System
    - sends SMS with a link to the Client
    - serves a Web app to hanlde clicks to the link provided
  - Client
    - receives the SMS
    - clicks the link
    - allows location sharing in their browser
  - Web App
    - sends client's location to the system
  - System
    - updates client's location in database
    - notifies call-center operator

Parts of the system involved in the process:
  - database tables
    - `location_sharing_request` − records requests sent to clients
      - url sent in SMS
      - case identifier
    - `location_sharing_response` − records responses from clients
      - case identifier
      - shared location
  - carma frontend
    - provides a button to request location
    - sends `POST /_/request_location` when the button is pressed
  - carma server
    - handles POST requests to `/_/request_location`
    - inserts new request into `location_sharing_request` table
  - location-sharing-svc
    - waits for a new record in the `location_sharing_request` table
    - relies on sms-svc service to send SMS
    - serves Web App
    - handles `POST /location` from the Web App
    - calls carma server via HTTP API to update client's location in database

To implement the service we rely on the following technologies and libraries:
  - `servant` HTTP server
    - already used in carma-nominatim-mediator and carma-era-glonass-integration
    - well maintained and has bigger community (compared to `snap` and `scotty`
      that are used in other parts of the system)
  - `heist` for HTML teplates (as it is used in carma server)
  - `persistent` and `esqueleto` for typed SQL queries
