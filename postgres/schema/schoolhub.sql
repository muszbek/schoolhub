CREATE TYPE privilege AS ENUM('student', 'teacher', 'admin');
CREATE TABLE user_privileges(
    username varchar(250) PRIMARY KEY,
    permission privilege
);
