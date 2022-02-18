module TOP2

import iTasks
import iTasks.Extensions.DateTime  // to include comparison on DateTime
import Text.HTML

from StdOverloadedList import class List
from StdLib import partition

:: Job
 = { whatToDo :: String
   , deadline :: ?DateTime
   , who      :: [UserId]
   , id       :: Int
   }
derive class iTask Job

deadline :: Job -> ?DateTime
deadline {Job | deadline} = deadline

who :: Job -> [UserId]
who {Job | who} = who

iden :: Job -> Int
iden {Job | id} = id

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
                        , workflow "View open jobs" "view open jobs of all group members" viewAllJobs
                        , workflow "View done jobs" "view all jobs that have been performed" viewDoneJobs
                        , workflow "View neglected jobs" "view all jobs whose deadline has passed" viewNeglectedJobs
                        , workflow "View my jobs" "view all jobs for the current user" viewMyJobs
                        , workflow "Create a new job" "Creates a new job" createJob
                        , workflow "Do a job" "Do a job that is assigned to you" doAJob
                        , workflow "Clean jobs" "Remove all jobs whose deadlines have expired" cleanJobs
                        // the list of tasks
                        ]
     , loginMessage   = ?None // optional login message
     , welcomeMessage = ?None // optional welcome message
     , allowGuests    = False // this application does not make sense for anonymous users
     } world

viewMembers :: Task [User]
viewMembers
 = viewSharedInformation [] users


// 1.
viewAllJobs :: Task [Job]
viewAllJobs = viewSharedInformation [] openJobsSDS
              <<@ Title "All open jobs"


// 2.
viewDoneJobs :: Task [(UserId, Job)]
viewDoneJobs = viewSharedInformation [] doneJobsSDS
              <<@ Title "Finished jobs"


// 3.
viewNeglectedJobs :: Task[Job]
viewNeglectedJobs = viewSharedInformation [] neglectedJobsSDS
              <<@ Title "Neglected jobs"


// 4.
userId :: User -> ?UserId
userId (AuthenticatedUser id _ _) = ?Just id
userId _ = ?None

isMaybeMember :: (? a) [a] -> Bool | Eq a
isMaybeMember (?Just(x)) xs = isMember x xs
isMaybeMember (?None) _     = False

viewMyJobs :: Task (User, [Job])
viewMyJobs = viewSharedInformation [ ViewAs (\(user, jobs) = filter (isAssignedTo user) jobs) ] (currentUser |*| openJobsSDS)
  where isAssignedTo u j = isMaybeMember (userId u) (who j)

// 5.
selectUsers :: Task [User]
selectUsers = enterMultipleChoiceWithShared [] users
    <<@ Hint "Select at least 1 user to assign to the task"

enterJobDescription :: Task String
enterJobDescription = enterInformation []
    <<@ Hint "Describe the task to be performed"

// i.e. 2022-02-16 21:43:00
enterJobDeadline :: Task (? DateTime)
enterJobDeadline = 
    Hint "Enter a deadline for the task or cancel to confirm without deadline" @>>
    enterInformation [] >>*
    [
        OnAction ActionOk (hasValue (return o ?Just))
      , OnAction ActionCancel (always (return ?None))
    ]

createJob :: Task [Job]
createJob =  selectUsers 
          >>* [OnAction ActionOk (ifValue (\us = length us > 0) return)]
          >>- \assignees = enterJobDescription
          >>? \descrition = enterJobDeadline
          >>? \deadline = upd (\js = [{Job |
                                        whatToDo = descrition, 
                                        deadline = deadline, 
                                        who=(catMaybes o map userId) assignees, 
                                        id = unique_job_id js } : js])
                                                                  openJobsSDS
          where unique_job_id js = hd [i \\ i <- [0..(length js)+1] | not (any (\j = i == j) (ids js) ) ]
                ids js = map iden js

// 6.
selectFromMyJobs :: Task Job
selectFromMyJobs = get currentUser 
                >>- \user = get openJobsSDS
                >>- \jobs = get currentDateTime
                >>- \now  = 
                Hint "Choose a job to do" @>>
                enterChoice [ChooseFromCheckGroup id ] (myFutureJobs user jobs now)
where myFutureJobs u js now = filter (\j = isAssignedTo u j && inFuture (deadline j) now) js
      isAssignedTo u j = isMaybeMember (userId u) (who j)
      inFuture (?None) _      = True
      inFuture (?Just(d)) now = d < now

confirmJobDone :: Job -> Task Job
confirmJobDone job = Title "Confirm the job was finished" @>>
                viewInformation [] job

updateDoneJobs :: Job -> Task ([Job], [(UserId, Job)])
updateDoneJobs job = get currentUser  
                >>- \user = upd (\(openJobs, doneJobs) = (filteredJobs job openJobs, [((performedById user), job): doneJobs])) (openJobsSDS >*< doneJobsSDS)
  where filteredJobs oldJob jobs = filter (\j = iden j <> iden oldJob) jobs
        performedById u = case userId u of
            ?Just(x) = x
            ?None = "Anonymous"

doAJob :: Task ([Job], [(UserId, Job)])
doAJob = selectFromMyJobs 
      >>? \job = confirmJobDone job
      >>* [OnAction (Action "Confirm") (always (updateDoneJobs job))]

// 7.
isExpired :: (? DateTime) DateTime -> Bool
isExpired (?None) _      = False
isExpired (?Just(d)) now = d <= now

splitExpiredJobs :: [Job] DateTime -> ([Job], [Job])
splitExpiredJobs jobs now = partition (\j = isExpired (deadline j) now) jobs

cleanJobs :: Task ([Job], [Job])
cleanJobs = get currentDateTime
          >>- \now = upd (\(openJobs, doneJobs) = 
          let (expired, open) = splitExpiredJobs openJobs now in 
              (open, expired ++ doneJobs)) 
                  (openJobsSDS >*< neglectedJobsSDS)
