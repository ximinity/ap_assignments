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

// 1
// Start w = doTasks enter_student w

// 2.
// Start w = doTasks enter_students w

// 3.
// Start w = doTasks (update_student (hd students)) w

// 4.
// Start w = doTasks (select_student students) w

// 5.
// Start w = doTasks (select_student_by_name students) w

// 6.
// Start w = doTasks (select_year_sorted students) w

// 7.
// Start w = doTasks (select_same_study 2021 (hd students) students) w

// 8.
// Start w = doTasks (select_same_study_from_population students) w

// 9.
// Start w = doTasks (update_studentnumber (hd students)) w

// 10.
Start w = doTasks (fix students) w

name :: Student -> String
name {Student | name} = name

regs :: Student -> [Registration]
regs {Student | regs} = regs

snum :: Student -> String
snum {Student | snum} = snum

enter_student :: Task Student
enter_student = enterInformation []

enter_students :: Task [Student]
enter_students = enterInformation []

update_student :: Student -> Task Student
update_student student = updateInformation [] student

select_student :: [Student] -> Task Student
select_student students = enterChoice [] students

select_student_by_name :: [Student] -> Task Student
select_student_by_name students = enterChoice [ChooseFromCheckGroup (\p = name p)] students

select_year_sorted :: [Student] -> Task Year
select_year_sorted students = enterChoice [ChooseFromGrid id] (sort years)
	where years = (removeDup o (map fst) o flatten o (map (\s = s.Student.regs))) students

select_same_study :: Year Student [Student] -> Task [Student]
select_same_study year student students
 = Title title @>> viewInformation [] students1
	where
		studentsInYear = filter (\x = isInYear year x && snum x <> snum student) students
		isInYear y s = isMember y (((map fst) o regs) s)
		(title, students1)
			= if (isMember year (((map fst) o regs) student))
				(case studentsInYear of
					[] -> ("No other students found in the same year", [])
					ss -> ("Found the following students", ss)
				)
				("Student does not have a registration for the given year", [])

select_same_study_from_population :: [Student] -> Task [Student]
select_same_study_from_population students
 = (select_student_by_name students
		-&&-
	select_year_sorted students
   ) >>? (\(student, year) = select_same_study year student students)

valid_studentnumber :: String -> Bool
valid_studentnumber s = (valid_studentnumber1 o fromString) s
	where
		valid_studentnumber1 [] = False
		valid_studentnumber1 ['s' : cs] = length cs == 4 && all isDigit cs
		valid_studentnumber1 _ = False

update_studentnumber :: Student -> Task Student
update_studentnumber student
 = (
	 (
		 Title "Update student number" @>>
		 (Hint "Student number must start with an s followed by four digits, e.g. s1234" @>>
		 enterInformation []
	 )
		-||
	 (Title "Current information: " @>> viewInformation [] student)
   )
	>>* [OnAction ActionOk (ifValue valid_studentnumber return)])
	>>- (\snum1 = Title "Updated information" @>> viewInformation [] { name = name student, snum = snum1, regs = regs student })

unique_studentnumber :: String [Student] -> Bool
unique_studentnumber num students = (foldr (\a b = if (snum a == num) (b + 1) b) 0 students) <= 1

fix :: [Student] -> Task [Student]
fix students = case (duplicate_snum_students students) of
        [] -> Title "Fixed student list" @>> viewInformation [] students
        xs -> Title "Update non-unique student numbers for: " @>>
                        enterChoice [ChooseFromGrid (\(s, _) = s)] xs
                                >>?
                        (\(old_s, i) = update_studentnumber old_s
                                >>?
                        (\new_s = fix [new_s : (removeAt i students)] ))

duplicate_snum_students :: [Student] -> [(Student, Int)]
duplicate_snum_students students = [(s, i) \\ (s, i) <- indexed_students, d <- duplicate_student_numbers | s.Student.snum == d]
        where
			duplicate_student_numbers = removeDup [num \\ num <- snums | (length (filter ((==) num) snums)) > 1]
            snums = map (\s = s.snum) students
            indexed_students = zip2 students [0..]
