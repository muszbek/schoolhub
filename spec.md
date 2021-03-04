- The use of the platform is segmented by (course) modules.
- Anyone can register and set their title voluntairily. Default is student. (Maybe change later that need special access to change.)
  - PostgreSQL
  
- Users with teacher rights can create a module and invite others to participate.
- The creator is admin.
- The admin can extend the privileges of others.
- The admin can give module description.
  - Elixir, PostgreSQL

- Send private messages to other person.
  - MongooseMQ XMPP, frontend? Elixir with Escalus?

- Message board with reply threads.
- Admin can pin messages (questions) as important, FAQ.
- Admin can delete or modify messages.
  - REST, Elixir, (maybe later MongoosePush)

- Admin can upload files (on message board, also library).
- Files are downloadable or view only -> view pdf or other stuff in browser
  - Elixir, PostgreSQL

- Grades on student page.
- Admin uploads list.
- Or admin uploads one by one.
- Students receive it on their profile privately.
  - Elixir, MongooseMQ push

- Create polls. Close polls.
- Organizatinal permanent polls that show on message board.
- Poll page, for quick polls in class. Either persist or disappear.
  - Elixir, MongooseMQ push
  
- Create exams, submit files with deadline

#Client

Authenticate:
- TCP sockets gen_tcp? (mochiweb?)
- if authenticated, create session which is new server (or new supervisor?)

User rights:
global rights: admin / teacher / student
admin can reassign rights and create courses
teacher can create courses

course rights: owner / assistant / student
owner can reassign rights and modify courses
assistant can modify courses

#After review with Antrea:

grade is too hidden

posts -> announcements or questions
questions polling
reply to posts is confusing

forgot password
reg confirm password
email confirm reg

files assignments grades
exams time limit

questions: students ask, and "like" or "follow" the questions so they are weighed
this helps the teacher prioritize

sort questions by title, chapter (numeric)

invite users to course by spreading token, user regs themselves

go with kurbernetes server model
limit the users, find a way to monitor
