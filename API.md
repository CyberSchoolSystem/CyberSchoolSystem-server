# API Overview

## Voting
| URI | Request Method | Request Body Type | Request Body Keys | Return |
|-----|----------------|-------------------|---------------------|------|
|`/api/vote/info` | POST | `application/json` | Optional: "vid", "choice" | `[ vote ]` |
|`/api/vote/act` | POST | `application/json` | "username", "vid", "choice" | "error" |
|`/api/vote/add` | POST | `application/json` | "description", "choices" | `null` |
|`/api/vote/remove` | POST | `application/json` | "vid" | "error" | "error" |

## Access
| URI | Request Method | Request Body Type | Request Body Keys | Return |
|-----|----------------|-------------------|---------------------|------|
|`/api/access/in` | POST | `application/json` | "username" | "error" |
|`/api/access/out` | POST | `application/json` | "username" | "error" |

## Users
| URI | Request Method | Request Body Type | Request Body Keys | Return |
|-----|----------------|-------------------|---------------------|------|
|`/api/user/add`| POST | `application/json` | "firstname", "lastname", "gradeId" <br> Optional: "username", "password" | "error" |
|`/api/user/remove` | POST | `application/json` | "username" | "error" |
|`/api/user/update` | POST | `application/json` | "idUsername" <br> Optional: "firstName", "lastName", "gradeId", "username", "password" | "error" |
|`/api/user/info` | POST | `application/json` | Optional: "firstName", "lastName", "gradeId", "username" | `[ user ]` |
|`/api/user/grade/add` | POST | `application/json` | "grade" | "gradeId" |
|`/api/user/grade/info` | GET | | | `[ { "id": gradeId, "name": grade } ] ` |
|`/api/user/grade/remove` | POST | `application/json` | "gradeId" | "error" |


## Keys
`firstName`: String

`lastName`: String

`grade`: String

`gradeId`: String

`username`: String

`idUsername`: String

`password`: String

`vid`: String

`vote`: Object( `id`: String, `description`: String, `choices`: List of Objects (`votes`: Integer, `identity`: Integer, `description`: String))

`error`: Either `null` or Object (`missingField`: List o. strings, `wrongFieldValue`: List o. String value tuples, `impossible`: S. `wrongFieldValue`, `permissionDenied`: Value)

`user`: Object. (`firstName`: String, `lastName`: String, `grade`: Text, `username`: Text, `fails`: Int, `roles`: roles,)

`roles`: Object (`citizen`: Bool, `representative`: Bool, `admin`: Bool, `teacher`: null or Text)

`choice`: Integer

`choices`: Array of Strings

`description`: String
