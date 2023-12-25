.read fa19data.sql

CREATE TABLE obedience AS
  SELECT seven, instructor FROM students;

CREATE TABLE smallest_int AS
  SELECT time, smallest FROM students WHERE smallest > 2 ORDER BY smallest LIMIT 20;

CREATE TABLE matchmaker AS
  SELECT t1.pet, t1.song, t1.color, t2.color FROM students AS t1, students AS t2 WHERE t1.pet = t2.pet AND t1.song = t2.song AND t1.time != t2.time;
