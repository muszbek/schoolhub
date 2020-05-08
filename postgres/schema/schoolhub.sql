CREATE TYPE privilege AS ENUM('student', 'teacher', 'admin');
CREATE TABLE user_privileges(
    username varchar(250) PRIMARY KEY,
    permission privilege
);

CREATE TABLE courses(
    id SERIAL UNIQUE PRIMARY KEY,
    name varchar(250) UNIQUE NOT NULL,
    owner varchar(250) NOT NULL,
    description jsonb,
    active boolean
);

CREATE TYPE course_affiliation AS ENUM('student', 'assistant', 'owner');
CREATE TABLE course_affiliations(
    username varchar(250) PRIMARY KEY,
    course int NOT NULL,
    affiliation course_affiliation,
    grades jsonb
);
