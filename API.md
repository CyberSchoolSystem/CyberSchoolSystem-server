# API Overview

## Voting
| URI | Request Method | Request Body Type | Request Body Content|
|-----|----------------|-------------------|---------------------|
|`/api/vote/act` | POST | `application/json` | `{ "chip": "CHIPID", "vid": VOTEID, "choice": CHOICEID }` |
|`/api/vote/add` | POST | `application/json` | `{ "description": "DESCRIPTION", "choices": [ "CHOICE" ] }`|
|`/api/vote/remove` | POST | `application/json` | `{ "vid": VOTEID }` |

## Access
| URI | Request Method | Request Body Type | Request Body Content|
|-----|----------------|-------------------|---------------------|
|`/api/access/in` | POST | `application/json` | `{ "chip": "CHIPID" }` |
|`/api/access/out` | POST | `application/json` | `{ "chip": "CHIPID" }` |

`CHIPID`: String
`VOTEID`: Number
`CHOICEID`: Number
`DESCRIPTION`: String
`CHOICE`: String
