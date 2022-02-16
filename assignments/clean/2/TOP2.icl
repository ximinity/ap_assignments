module TOP2

import iTasks
import iTasks.Extensions.DateTime  // to include comparison on DateTime
import Text.HTML

:: Job
 = { whatToDo :: String
   , deadline :: ?DateTime
   , who      :: [UserId]
   }
derive class iTask Job

openJobsSDS :: SimpleSDSLens [Job]
openJobsSDS = sharedStore "openjobs" []

doneJobsSDS :: SimpleSDSLens [(UserId,Job)]
doneJobsSDS = sharedStore "donejobs" []

neglectedJobsSDS :: SimpleSDSLens [Job]
neglectedJobsSDS = sharedStore "neglectedjobs" []

Start :: *World -> *World
Start world
 = doTasks 
     { WorkflowCollection
     | name           = "Top2" // a name for your application
     , workflows      = [ workflow "Manage users" "manage users"              manageUsers
                        , workflow "View members" "view current group member" viewMembers
                        // the list of tasks
                        ]
     , loginMessage   = ?None // optional login message
     , welcomeMessage = ?None // optional welcome message
     , allowGuests    = False // this application does not make sense for anonymous users
     } world

viewMembers :: Task [User]
viewMembers
 = viewSharedInformation [] users
