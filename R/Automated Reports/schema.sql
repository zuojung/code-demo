-- Drop all tables if it already exists
DROP TABLE IF EXISTS Neighborhood CASCADE;
DROP TABLE IF EXISTS section CASCADE;
DROP TABLE IF EXISTS crime CASCADE;
DROP TABLE IF EXISTS Neighborhood CASCADE;


create table Neighborhood (
       neighborhood_id serial primary key,
	   intptlat10 numeric,
	   intptlon10 numeric,
	   hood text NOT NULL,
	   hood_no integer,
	   acres numeric,
	   sqmiles numeric
	   );
	   
-- Import neighborhood data from server to sql database
\copy Neighborhood (intptlat10,intptlon10,hood,hood_no,acres,sqmiles) FROM '/home/zuojung/problem-bank/Data/police-neighborhoods.csv' WITH DELIMITER ',' csv header;


create table section (
       section_id serial primary key,
	   code text,
	   crime_type text
	   );
	   
insert into section (code, crime_type)
values ('3304', 'Criminal mischief'),
	   ('2709', 'Harassment'),
	   ('3502', 'Burglary'),
	   ('13(a)(16)', 'Possession of a controlled substance'),
	   ('13(a)(30)', 'Possession w/ intent to deliver'),
	   ('3701', 'Robbery'),
	   ('3921', 'Theft'),
	   ('3921(a)', 'Theft of movable property'),
	   ('3934', 'Theft from a motor vehicle'),
	   ('3929', 'Retail theft'),
	   ('2701', 'Simple assault'),
	   ('2702', 'Aggravated assault'),
	   ('2501', 'Homicide');
	  
create table crime (
       crime_id serial primary key,
	   report_name text,
	   section_id integer NOT NULL REFERENCES section,
	   description text,
	   arrest_time timestamp,
	   address text NOT NULL,
	   neighborhood_id integer NOT NULL REFERENCES Neighborhood,
	   zone integer check (zone > 0)
	   );
	   
create table counter (
	id serial primary key, 
	ts timestamp without time zone default (now() at time zone 'utc'),
	total_loaded integer,
	total_skipped integer,
	total_corrected integer,
	total_patched integer,
	total_not_patched integer
	);

insert into counter (total_loaded, total_skipped, total_corrected, total_patched, total_not_patched) values (0,0,0,0,0);