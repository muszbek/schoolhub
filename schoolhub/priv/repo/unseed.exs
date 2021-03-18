# Script for clearing the database. You can run it as:
#
#     mix run priv/repo/unseed.exs

schemas = [Schoolhub.Files.FileData,
	   Schoolhub.Files.File,
	   Schoolhub.Posts.Reply,
	   Schoolhub.Posts.Post,
	   Schoolhub.Questions.Follow,
	   Schoolhub.Questions.Qreply,
	   Schoolhub.Questions.Question,
	   Schoolhub.Grades.Grade,
	   Schoolhub.Courses.Affiliation,
	   Schoolhub.Courses.Course,
	   Schoolhub.Accounts.User,
	   Schoolhub.Accounts.Credential,
	   Schoolhub.Privileges.Privilege]

Schoolhub.Repo.truncate(schemas)
