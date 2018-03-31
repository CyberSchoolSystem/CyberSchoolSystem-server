# API Overview

## Voting
| URI | Request Method | Request Body Type | Request Body Keys |
|-----|----------------|-------------------|---------------------|
|`/api/vote/info` | POST | `application/json` | Optional: "vid", "choice"
|`/api/vote/act` | POST | `application/json` | "chip", "vid", "choice" |
|`/api/vote/add` | POST | `application/json` | "description", "choices" |
|`/api/vote/remove` | POST | `application/json` | "vid" |

## Access
| URI | Request Method | Request Body Type | Request Body Keys |
|-----|----------------|-------------------|---------------------|
|`/api/access/in` | POST | `application/json` | "chip" |
|`/api/access/out` | POST | `application/json` | "chip" |

## Users
| URI | Request Method | Request Body Type | Request Body Keys |
|-----|----------------|-------------------|---------------------|
|`/api/user/add`| POST | `application/json` | "firstname", "lastname", "grade", "chip" <br> Optional: "username", "password"
|`/api/user/remove` | POST | `application/json` | "uid"
|`/api/user/update` | POST | `application/json` | "uid" <br> Optional: "firstName", "lastName", "grade", "chip", "username", "password"
|`/api/user/info` | POST | `application/json` | Optional: "firstName", "lastName", "grade", "chip", "username"


## Keys
`firstName`: String

`lastName`: String

`grade`: String

`chip`: String (?)

`username`: String

`password`: String

`uid`: Object. Keys: Either "chip" or "username"

`vid`: Integer

`choice`: Integer

`choices`: Array of Strings

`description`: String
