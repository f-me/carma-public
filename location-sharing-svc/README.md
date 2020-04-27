This service allows unfortunate car owners (Clients) to share their location
using Web app.

## How to build
  - FIXME
  - parcel
  - relies on database bits from 11-LocationSharing.sql

## How to test
  - FIXME


We use stored procedures as a concise API to DB tables. This allows more
granular control on permissions.
We use Postgresql notify/listen mechanism to communicate between this service
and the main carma service:
  - we rely on the existing medium to communicate between services;
  - notifications are part of transactions, so it is not possible to add
    request or response without notification or notify without adding.

## How it works
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
    - `LocationSharingRequest` − records requests sent to clients
      - url sent in SMS
      - case identifier
    - `LocationSharingResponse` − records responses from clients
      - case identifier
      - shared location
  - carma frontend
    - provides a button to request location
    - sends `POST /requestLocation` when the button is pressed
    - show UI notifications about requests and responses
  - carma server
    - handles POST requests to `/requestLocation`
    - inserts new request into `LocationSharingRequest` table
    - handles location responses and updates location
  - location-sharing-svc
    - waits for a new record in the `LocationSharingRequest` table
    - relies on sms-svc service to send SMS
    - serves Web App
    - handles `POST /location` from the Web App
    - adds response to `LocationSharingResponse` table
    - sends PG notification to the main carma service
