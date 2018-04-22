# API Overview

## Voting
| URI | Request Method | Request Body Type | Request Body Keys | Return |
|-----|----------------|-------------------|---------------------|------|
|`/api/vote/info` | POST | `application/json` | Optional: "vid", "choice" | `[ vote ]` |
|`/api/vote/vote` | POST | `application/json` | "vid", "choice" | "error" |
|`/api/vote/add` | POST | `application/json` | "description", "choices", "endOfLife", "title"| `null` |
|`/api/vote/remove` | POST | `application/json` | "vid" | "error" | "error" |

## Access
| URI | Request Method | Request Body Type | Request Body Keys | Return |
|-----|----------------|-------------------|---------------------|------|
|`/api/access/in` | POST | `application/json` | "username" | "error" |
|`/api/access/out` | POST | `application/json` | "username" | "error" |

## Users
| URI | Request Method | Request Body Type | Request Body Keys | Return |
|-----|----------------|-------------------|---------------------|------|
|`/api/user/add`| POST | `application/json` | "firstname", "lastname", "gradeId", "Username","role" <br> Optional: "password" | "error" |
|`/api/user/remove` | POST | `application/json` | "username" | "error" |
|`/api/user/update` | POST | `application/json` | "idUsername" <br> Optional: "firstName", "lastName", "gradeId", "username", "password" | "error" |
|`/api/user/info` | POST | `application/json` | Optional: "firstName", "lastName", "gradeId", "username" | `[ user ]` |
|`/api/user/grade/add` | POST | `application/json` | "grade" | "gradeId" or error |
|`/api/user/grade/info` | GET | | | `[ { "id": gradeId, "name": grade } ] ` |
|`/api/user/grade/remove` | POST | `application/json` | "gradeId" | "error" |
|`/api/user/self/setpw` | POST | `application/json` | "password" | "error" |
|`/api/user/self/info` | GET | | | "userPriv" |


## Keys
`firstName`: String

`lastName`: String

`grade`: String

`gradeId`: String

`username`: String

`idUsername`: String

`password`: String

`vid`: String

`vote`: Object( `id`: String, `endOfLife`: String (Format: `2018-08-08T19:20:20Z`) `description`: String, (If EOL reached: `voted`: Int) `choices`: List of Objects, ((`votes`: Integer, if eol reached), `identity`: Integer, `description`: String))

`error`: Either `null` or Object (`missingField`, `timedOut`, `alreadyDone`, `unknown`, `notUnique` all a object (`msg`: String, `field`: List of fields (+values) that raised the error) `internalError`, `invalidArgs`,
`permissionDenied`, `badMethod` object (`msg`: String, `info`: string), `notFound`, `notAuthenticated` object (`msg`: String))

`user`: Object. (`firstName`: String, `lastName`: String, `gradeId`: Text, `username`: Text, `roles`: roles,)

`userPriv`: Object. (`firstName`: String, `lastName`: String, `gradeId`: Text, `username`: Text, `fails`: Int, `roles`: roles, `fails`: Int, `access`: `[ (Bool, Time)]`)

`role`: Object (`citizen`: Bool, `representative`: Bool, `admin`: Bool, `teacher`: null or Text, `tech`: Bool, `customs`: Bool)

`choice`: Integer

`choices`: Array of Strings

`description`: String

`endOfLife`: String. Time format "YYYY-MM-DD HH:MM:SS Z" (Z: Timezone. e.g `Z` (UTC) `(+1)` UTC +1)
