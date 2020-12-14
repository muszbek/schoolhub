# Script for populating the database. You can run it as:
#
#     mix run priv/repo/demo_seeds.exs

## Add accounts
alias Schoolhub.Accounts

Accounts.create_user(%{name: "Tamas Muszbek",
		       email: "muszbektamas@gmail.com",
		       credential: %{username: "tmuszbek",
				     password: "tmuszbek"}})

Accounts.create_user(%{name: "Antrea Pavlou",
		       email: "antreapavlou@gmail.com",
		       credential: %{username: "apavlou",
				     password: "apavlou"}})

Accounts.create_user(%{name: "Nassab Reda",
		       email: "nassabreda@gmail.com",
		       credential: %{username: "nreda",
				     password: "nreda"}})

Accounts.create_user(%{name: "Maaike Sangster",
		       email: "maaikesangster@gmail.com",
		       credential: %{username: "msangster",
				     password: "msangster"}})

Accounts.create_user(%{name: "Saad Boutayeb",
		       email: "saadboutayeb@gmail.com",
		       credential: %{username: "sboutayeb",
				     password: "sboutayeb"}})

Accounts.create_user(%{name: "Dominique Pollux",
		       email: "dominiquepollux@gmail.com",
		       credential: %{username: "dpollux",
				     password: "dpollux"}})

Accounts.create_user(%{name: "Florence Dode",
		       email: "florencedode@gmail.com",
		       credential: %{username: "fdode",
				     password: "fdode"}})

Accounts.create_user(%{name: "Gabriel Benevides",
		       email: "gabrielbenevides@gmail.com",
		       credential: %{username: "gbenevides",
				     password: "gbenevides"}})

Accounts.create_user(%{name: "Jaideep Patil",
		       email: "jaideeppatil@gmail.com",
		       credential: %{username: "jpatil",
				     password: "jpatil"}})

Accounts.create_user(%{name: "Romain Gruson",
		       email: "romaingruson@gmail.com",
		       credential: %{username: "rgruson",
				     password: "rgruson"}})


## Elevate privilege of certain accounts
alias Schoolhub.Accounts.User
alias Schoolhub.Privileges.Privilege
alias Schoolhub.Privileges

%User{privilege: privilege = %Privilege{}} = Accounts.get_user_by_name!("apavlou")
Privileges.update_privilege(privilege, %{level: "teacher"})

%User{privilege: privilege = %Privilege{}} = Accounts.get_user_by_name!("tmuszbek")
Privileges.update_privilege(privilege, %{level: "teacher"})


## Add courses
alias Schoolhub.Courses
alias Schoolhub.Courses.Course

%User{id: creator} = Accounts.get_user_by_name!("tmuszbek")
title = "Introduction to Computer Science and Programming"
desc = "About This Course

Let's start with the strategic goals of this course:

    Help students (who may or may not intend to major in computer science) to feel justifiably confident of their ability to write small programs.
    Map scientific problems into computational frameworks.
    Position students so that they can compete for jobs by providing competence and confidence in computational problem solving.
    Prepare college freshmen and sophomores who have no prior programming experience or knowledge of computer science for an easier entry into computer science or electrical engineering majors.
    Prepare students from other majors to make profitable use of computational methods in their chosen field.

6.00SC can be summarized with these six major topics or objectives:

    Learning a language for expressing computations—Python
    Learning about the process of writing and debugging a program
    Learning about the process of moving from a problem statement to a computational formulation of a method for solving the problem
    Learning a basic set of \"recipes\"—algorithms
    Learning how to use simulations to shed light on problems that don't easily succumb to closed form solutions
    Learning about how to use computational tools to help model and understand data

6.00 is designed to help you become skillful at making the computer do what you want it to do. Once you acquire this skill, your first instinct when confronted with many tasks will be to write a program to do the task for you. Said another way, we want to help you learn to apply computational modes of thought to frame problems, and to guide the process of deducing information in a computational manner.

This means that the primary knowledge you will take away from this course is the art of computational problem solving. Unlike many introductory level courses, having an ability to memorize facts will be of little help in 6.00. This course is about learning to solve problems, not learning facts. (This, by the way, is exactly why all exams are open book.)

Prerequisites and Preparation

This course is aimed at students with little or no prior programming experience but a desire to understand computational approaches to problem solving. Now, by definition, none of you are under-qualified for this course. In terms of being over-qualified — if you have a lot of prior programming experience, we really don't want you wasting your time, and in this case we would suggest that you talk to me about how well this class suits your needs, and to discuss other options. In addition, we want to maintain a productive educational environment, and thus we don't want over-qualified students making other students feel inadequate, when in fact they are only inexperienced.

Since computer programming involves computational modes of thinking, it will help to have some mathematical and logical aptitude. You should be confident with your math skills up to pre-calculus.
Textbook

The original textbook for 6.00 and the course lectures parallel each other, though there is more detail in the book about some topics. The book is NOT required. We will not be referring to it in assignments or depending upon it to cover holes in the lectures.

Buy at MIT Press Buy at Amazon Guttag, John. Introduction to Computation and Programming Using Python. Spring 2013 edition. MIT Press, 2013. ISBN: 9780262519632.

A second edition of the textbook is now available. However, there may be some discrepancies between the original course lectures included on this course site and the sections in this second edition of the textbook.

Buy at MIT Press Buy at Amazon Guttag, John. Introduction to Computation and Programming Using Python: With Application to Understanding Data. MIT Press, 2016. ISBN: 9780262529624.

If you choose not to purchase the textbook, you will probably find it useful to buy or borrow another book that covers Python. You might check your local public library's resources, or search online for a free Python text, such as How to Think Like a Computer Scientist or This resource may not render correctly in a screen reader.An Introduction to Python (PDF).

Online readings will be posted on the appropriate session pages. A more complete list of readings and references (not all of which are specifically assigned during lectures) can be found in the References section.

Technical Requirements

Since one of the goals of this course is to become familiar with programming, you will need to install and use the Python programming language and the interpreter IDLE. Please see the Software section for information and instructions on downloading the required software.

Most lectures involve programming demonstrations, and the code involved will generally be posted twice: once as a handout in PDF format, and again as a code file in .PY (Python) format. Additionally, many problem sets have accompanying code required for completing the assignment, and these are posted as .PY (Python) files. If you do not have the software installed, you will not be able to properly open and use these files.

Acknowledgments

We would like to thank course TAs Mitchell Peabody, Gartheeban Ganeshapillai, and Sarina Canelake for their participation in filming 6.00 recitations for OCW Scholar, and Niki Castle and Elaina Cherry for their work and dedication adapting the 6.00 materials for Scholar students. We would also like to thank Eric Grimson for his role in the development of 6.00 teaching material over the years, and for allowing us to record a guest lecture."

{:ok, %Course{id: course1}} =
  Courses.create_course(%{name: title, description: desc, creator: creator, active: true})


%User{id: creator} = Accounts.get_user_by_name!("apavlou")
title = "User Interface Design and Implementation"
desc = "Course Meeting Times

Lectures: 3 sessions / week, 1 hour / session

Prerequisites

6.005 Elements of Software Construction

Course Overview

6.813/6.831 introduces the principles of user interface development, focusing on the following areas:

Design

We will look at how to design good user interfaces, covering important design principles (learnability, visibility, error prevention, efficiency, and graphic design) and the human capabilities that motivate them (including perception, motor skills, color vision, attention, and human error).

Implementation

We will see techniques for building user interfaces, including low-fidelity prototypes, Wizard of Oz, and other prototyping tools; input models, output models, model-view-controller, layout, constraints, and toolkits.

Evaluation

We will learn techniques for evaluating and measuring interface usability, including heuristic evaluation, predictive evaluation, and user testing.

Research

We will learn how to conduct empirical research involving novel user interfaces (graduate level only).

Textbooks

There is no required textbook for this course, but a list of Recommended Textbooks is available for students who wish to further explore the subject.

Collaboration

You may discuss assignments with other people, but you are expected to be intellectually honest and give credit where credit is due. In particular, for all individual assignments:

    you should write your solutions entirely on your own;
    you should not share written materials or code with anyone else;
    you should not view any written materials or code created by anyone else for the assignment;
    you should list all your collaborators (everyone you discussed the assignment with) on your hand in.

Grading

ACTIVITIES 	PERCENTAGES
Course project (GR1–GR6) 	42%
Problem sets (HW, PS/RS) 	30%
Nanoquizzes (unavailable on OCW) 	24%
Class participation 	4%

The largest contribution to your grade will be the course project (42%), in which you will work in small groups to design, implement, and evaluate a user interface, through an iterative design process with a series of graded milestones (GR1–GR6). Students from 6.813 and 6.831 may work in the same group.

Five problem sets (HW, PS/RS) will be assigned, which you must complete individually, not in a group. HW1–2 (\"homeworks\") are assigned to both courses; PS1–3 (\"programming\") are assigned only to the undergraduate course 6.813; and RS1–3 (\"research\") are assigned only to the graduate course 6.831. These five assignments will constitute 30% of your grade.

Every lecture will begin with a \"nanoquiz,\" which covers the content of the previous lecture or two. There will be approximately 30 nanoquizzes, which altogether count for 24% of your grade. If you miss class, no makeup quiz is offered. However, we will automatically drop your lowest 6 quiz grades, so that you have flexibility to miss class when necessary. (Nanoquizzes unavailable on MIT OpenCourseWare.)

There will be no other in-class quizzes, no midterm, and no final exam.

Participation in lecture, in-class activities, and project group meetings with course staff will also be a factor in your grade (4%)."

{:ok, %Course{id: course2}} =
  Courses.create_course(%{name: title, description: desc, creator: creator, active: true})


## Affiliating accounts to courses
%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Courses.create_affiliation(%{affiliation: "owner", course_id: course1, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Courses.create_affiliation(%{affiliation: "assistant", course_id: course1, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Courses.create_affiliation(%{affiliation: "student", course_id: course1, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
Courses.create_affiliation(%{affiliation: "student", course_id: course1, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("fdode")
Courses.create_affiliation(%{affiliation: "student", course_id: course1, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
Courses.create_affiliation(%{affiliation: "student", course_id: course1, user_id: user_id})


%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Courses.create_affiliation(%{affiliation: "owner", course_id: course2, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Courses.create_affiliation(%{affiliation: "assistant", course_id: course2, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Courses.create_affiliation(%{affiliation: "student", course_id: course2, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("msangster")
Courses.create_affiliation(%{affiliation: "student", course_id: course2, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("sboutayeb")
Courses.create_affiliation(%{affiliation: "student", course_id: course2, user_id: user_id})

%User{id: user_id} = Accounts.get_user_by_name!("dpollux")
Courses.create_affiliation(%{affiliation: "student", course_id: course2, user_id: user_id})


## Creating posts to courses
alias Schoolhub.Posts
alias Schoolhub.Posts.Post

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
{:ok, %Post{id: post1}} = Posts.create_post(%{creator: user_id, course_id: course1, pinned: false,
  content: "Is this real life"})

%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
{:ok, %Post{id: post2}} = Posts.create_post(%{creator: user_id, course_id: course1, pinned: true,
  content: "Az éjjel soha nem érhet véget"})

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
{:ok, %Post{id: post3}} = Posts.create_post(%{creator: user_id, course_id: course1, pinned: false,
  content: "שלח לי מלאך"})

%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
{:ok, %Post{id: post4}} = Posts.create_post(%{creator: user_id, course_id: course1, pinned: false,
  content: "Белая армия, чёрный барон"})


## Replies to posts
%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Or is this just fantasy"})

%User{id: user_id} = Accounts.get_user_by_name!("fdode")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Caught in a landslide"})

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "There's no escape from reality"})

%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Open your eyes"})

%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Look up to the sky and see"})

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "I'm just a poor boy"})

%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "I need no love"})

%User{id: user_id} = Accounts.get_user_by_name!("fdode")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Because I'm easy come - easy go"})

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Little high - little low"})

%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Anywhere the wind blows"})

%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "Doesn't really matter to me"})

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Posts.create_reply(%{creator: user_id, parent_post: post1, content: "To me"})


%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Posts.create_reply(%{creator: user_id, parent_post: post2, content: "Varázsolj nekünk valami szépet"})

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Posts.create_reply(%{creator: user_id, parent_post: post2, content: "Repülj velünk a szerelem szárnyán"})

%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
Posts.create_reply(%{creator: user_id, parent_post: post2, content: "Indul az utazás, csak erre vártál"})


%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Posts.create_reply(%{creator: user_id, parent_post: post3,
		     content: "שלח לי מלאך 
		     שיקח, שיקח אותי ללב 
		     שאוהב ומחכה לי 
		     ורוצה בי כל הזמן"})

%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Posts.create_reply(%{creator: user_id, parent_post: post3,
		     content: "שלח לי מלאך 
		     שאותי ואותך הוא לא יקח 
		     למקום שאין בו 
		     מה שיש פה כל הזמן"})

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Posts.create_reply(%{creator: user_id, parent_post: post3,
		     content: "ולפעמים הכל נראה אותו דבר 
		     ולפעמים, פתאום, יש רגע מאושר"})

%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
Posts.create_reply(%{creator: user_id, parent_post: post3,
		     content: "שלח לי מלאך 
		     שישמח וידליק פה את האור 
		     וישמור על מה שיש לשמור פה 
		     כל הזמן"})

%User{id: user_id} = Accounts.get_user_by_name!("fdode")
Posts.create_reply(%{creator: user_id, parent_post: post3,
		     content: "סתם הזיות זה גן חיות 
		     אריות ונמרים ואחרים 
		     מנסים למצוא מקום לחיות בו 
		     כל הזמן"})

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
Posts.create_reply(%{creator: user_id, parent_post: post3,
		     content: "ולפעמים הכל נראה אותו דבר 
		     ולפעמים, פתאום, יש רגע מאושר"})


%User{id: user_id} = Accounts.get_user_by_name!("fdode")
Posts.create_reply(%{creator: user_id, parent_post: post4,
		     content: "Белая армия,чёрный барон 
		     Снова готовят нам царский трон, 
		     Но от тайги до британских морей 
		     Красная Армия всех сильней. "})

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
Posts.create_reply(%{creator: user_id, parent_post: post4,
		     content: "Так пусть же Красная 
		     Сжимает властно 
		     Свой штык мозолистой рукой, 
		     И все должны мы 
		     Неудержимо 
		     Идти в последний смертный бой! "})

%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Posts.create_reply(%{creator: user_id, parent_post: post4,
		     content: "Красная Армия,марш вперёд! 
		     Реввоенсовет нас в бой зовёт. 
		     Ведь от тайги до британских морей 
		     Красная Армия всех сильней! "})

%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Posts.create_reply(%{creator: user_id, parent_post: post4,
		     content: "Так пусть же Красная 
		     Сжимает властно 
		     Свой штык мозолистой рукой, 
		     И все должны мы 
		     Неудержимо 
		     Идти в последний смертный бой! "})


## Add files to the course
alias Schoolhub.Files
alias File, as: BuiltinFile

path = "./assets/static/images/schoolhub.png"
{:ok, binary_content} = BuiltinFile.read(path)
{:ok, %{size: size}} = BuiltinFile.stat(path)
%User{id: user_id} = Accounts.get_user_by_name!("tmuszbek")
Files.create_file(%{uploader: user_id, course_id: course1, filename: "schoolhub.png", size: size,
		    file_data: %{data: binary_content}})

path = "./README.md"
{:ok, binary_content} = BuiltinFile.read(path)
{:ok, %{size: size}} = BuiltinFile.stat(path)
%User{id: user_id} = Accounts.get_user_by_name!("nreda")
Files.create_file(%{uploader: user_id, course_id: course1, filename: "README.md", size: size,
		    file_data: %{data: binary_content}})

path = "./priv/repo/demo_seeds.exs"
{:ok, binary_content} = BuiltinFile.read(path)
{:ok, %{size: size}} = BuiltinFile.stat(path)
%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
Files.create_file(%{uploader: user_id, course_id: course1, filename: "demo_seeds.exs", size: size,
		    file_data: %{data: binary_content}})


## Add grades to course members
alias Schoolhub.Grades
alias Schoolhub.Grades.Grade
alias Schoolhub.Courses.Affiliation

grade = %{first_test: 20, second_test: 20, project: 10, total: 50}

%User{id: user_id} = Accounts.get_user_by_name!("apavlou")
%Affiliation{grade: %Grade{id: grade_id}} = Courses.get_affiliation_by_user!(course1, user_id)
Grades.add_grade(grade_id, grade)

%User{id: user_id} = Accounts.get_user_by_name!("gbenevides")
%Affiliation{grade: %Grade{id: grade_id}} = Courses.get_affiliation_by_user!(course1, user_id)
Grades.add_grade(grade_id, grade)

%User{id: user_id} = Accounts.get_user_by_name!("fdode")
%Affiliation{grade: %Grade{id: grade_id}} = Courses.get_affiliation_by_user!(course1, user_id)
Grades.add_grade(grade_id, grade)

%User{id: user_id} = Accounts.get_user_by_name!("jpatil")
%Affiliation{grade: %Grade{id: grade_id}} = Courses.get_affiliation_by_user!(course1, user_id)
Grades.add_grade(grade_id, grade)
