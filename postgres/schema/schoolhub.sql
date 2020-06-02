CREATE TYPE privilege AS ENUM('student', 'teacher', 'admin');
CREATE TABLE user_privileges(
    username varchar(250) PRIMARY KEY,
    permission privilege
);

CREATE UNIQUE INDEX user_privileges_index ON user_privileges (username);

CREATE TABLE courses(
    id SERIAL UNIQUE PRIMARY KEY,
    name varchar(250) UNIQUE NOT NULL,
    creator varchar(250) NOT NULL,
    description jsonb,
    created_at timestamp(6) without time zone NOT NULL DEFAULT current_timestamp,
    active boolean
);

CREATE UNIQUE INDEX courses_index ON courses (name);

CREATE TYPE course_affiliation AS ENUM('student', 'assistant', 'owner');
CREATE TABLE course_affiliations(
    username varchar(250) PRIMARY KEY,
    course int NOT NULL,
    affiliation course_affiliation,
    grades jsonb
);

CREATE UNIQUE INDEX course_affiliations_index ON course_affiliations (username, course);

CREATE EXTENSION ltree;

CREATE TABLE course_messages(
    id SERIAL UNIQUE PRIMARY KEY,
    course int NOT NULL,
    author varchar(250) NOT NULL,
    path ltree,
    message jsonb,
    created_at timestamp(6) without time zone NOT NULL DEFAULT current_timestamp,
    pinned boolean
);

CREATE INDEX course_messages_course_index ON course_messages (course)
       WHERE pinned IS TRUE;
CREATE INDEX course_messages_path_index ON course_messages USING GIST (path)
       WHERE pinned IS TRUE;
