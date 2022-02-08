module TOP1

import StdArray  // needed in case you wish to use an array comprehension, e.g. [c \\ c <-: str]
import iTasks

/*	Peter Achten, P.Achten@cs.ru.nl
	Advanced Programming assignment 1 (Task Oriented Programming).
	Use this in a project with environment iTasks.
*/

:: Student
 = { name :: String          // we put no restrictions on names
   , snum :: String          // must be formatted as one s followed by 4 digit chars (isDigit in StdChar is useful)
   , regs :: [Registration]  // all known registrations by this student
   }
:: Registration
 :== (Year,BaMa)
:: Year
 :== Int
:: BaMa = Ba | Ma

derive class iTask Student, BaMa  // unlocks the iTask machinery for these new data types

instance ==       BaMa where (==) p1 p2 = gEq{|*|} p1 p2
instance toString BaMa where toString p = hd (gText{|*|} AsSingleLine (?Just p))

Start w = doTasks task w

task :: Task [Int]
task = enterInformation []

students :: [Student]
students =
	[{name = "Alice"
	 ,snum = "s1000"   // this student number is deliberately equal to Dave and Frank's student number
	 ,regs = [(2018,Ba),(2019,Ba),(2020,Ba),(2021,Ma)]
	 }
	,{name = "Bob"
	 ,snum = "s1003"
	 ,regs = [(2018,Ba),(2019,Ba),(2020,Ba),(2021,Ma)]
	 }
	,{name = "Carol"
	 ,snum = "s1024"
	 ,regs = [(2017,Ba),(2018,Ba),(2019,Ba),(2020,Ma),(2021,Ma)]
	 }
	,{name = "Dave"
	 ,snum = "s1000"   // this student number is deliberately equal to Alice and Frank's student number
	 ,regs = [(2017,Ba),(2019,Ba),(2020,Ba),(2021,Ba)]
	 }
	,{name = "Eve"
	 ,snum = "s4096"
	 ,regs = [(2016,Ba),(2017,Ba),(2018,Ba),(2019,Ba),(2020,Ma),(2021,Ma)]
	 }
	,{name = "Frank"
	 ,snum = "s1000"   // this student number is deliberately equal to Alice and Dave's student number
	 ,regs = [(2017,Ba),(2018,Ba),(2019,Ba),(2020,Ma),(2021,Ma)]
	 }
	]

name :: Student -> String
name {Student | name} = name

regs :: Student -> [Registration]
regs {Student | regs} = regs

snum :: Student -> String
snum {Student | snum} = snum

enter_student :: Task Student
enter_student = abort "enter_student not yet implemented"

enter_students :: Task [Student]
enter_students = abort "enter_students not yet implemented"

update_student :: Student -> Task Student
update_student student = abort "update_student not yet implemented"

select_student :: [Student] -> Task Student
select_student students = abort "select_student not yet implemented"

select_student_by_name :: [Student] -> Task Student
select_student_by_name students = abort "select_student_by_name not yet implemented"

select_year_sorted :: [Student] -> Task Year
select_year_sorted students = abort "select_year_sorted not yet implemented"

select_same_study :: Year Student [Student] -> Task [Student]
select_same_study year student students
 = abort "select_same_study not yet implemented"

select_same_study_from_population :: [Student] -> Task [Student]
select_same_study_from_population students
 = abort "select_same_study_from_population not yet implemented"

update_studentnumber :: Student -> Task Student
update_studentnumber student
 = abort "update_studentnumber not yet implemented"

fix :: [Student] -> Task [Student]
fix students
 = abort "fix not yet implemented"
