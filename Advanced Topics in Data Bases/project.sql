-- psql -d database -f project.sql

-- Drop the table if it exists
DROP TABLE IF EXISTS public.pieces;
DROP TABLE IF EXISTS public.puzzles;
DROP TABLE IF EXISTS public.solver;

-- Create the public.pieces table
CREATE TABLE public.pieces (
    id SERIAL PRIMARY KEY,
    color TEXT,
    shape GEOMETRY(POLYGON)
);

CREATE TABLE public.puzzles (
    id SERIAL PRIMARY KEY,
    color TEXT,
    shape GEOMETRY(POLYGON)
);

CREATE TABLE public.solver (
    id SERIAL PRIMARY KEY,
    id_puzzle INT,
    id_piece INT,
    pT GEOMETRY(POINT),
    pR FLOAT
);

-- Insert data using the calculated square root
INSERT INTO public.pieces (color, shape)
SELECT
  'red',
  ST_GeomFromText(
    'POLYGON((0 0, 0 ' || (sqrt(2)/2) || ', ' || -(sqrt(2)/2) || ' ' || (sqrt(2)/2) || ', 0 0))'
  );

INSERT INTO public.pieces (color, shape)
SELECT
  'orange',
  ST_GeomFromText(
    'POLYGON((0 0, ' || (sqrt(2)/2) || ' 0, ' || (sqrt(2)/2) || ' ' || (sqrt(2)/2) || ', 0 0))'
  );

INSERT INTO public.pieces (color, shape)
VALUES
  ('blue', ST_GeomFromText('POLYGON((0 0, 0.5 0, 0.5 0.5, 0 0))'));

INSERT INTO public.pieces (color, shape)
SELECT
  'pink',
  ST_GeomFromText(
    'POLYGON((0 0, 0.5 0, 0.25 0.25, 0 0))'
  );

INSERT INTO public.pieces (color, shape)
SELECT
  'purple',
  ST_GeomFromText(
    'POLYGON((0 0, 0.5 0, 0.25 0.25, 0 0))'
  );

INSERT INTO public.pieces (color, shape)
SELECT
  'yellow',
  ST_GeomFromText(
    'POLYGON((0 0, 0.25 0.25, 0 0.5, -0.25 0.25, 0 0))'
  );

INSERT INTO public.pieces (color, shape)
VALUES
  ('green', ST_GeomFromText('POLYGON((0 0, 0.5 0, 0.25 0.25, -0.25 0.25, 0 0))'));


--INSERT public.puzzles
INSERT INTO public.puzzles (color, shape) -- puzzle 1
SELECT
  'black',
  ST_GeomFromText(
    'POLYGON((0 0, 1 0, 0.75 0.25, 1 0.5, 0.75 0.75, '|| (1+sqrt(2))/2 || ' ' || (1+sqrt(2))/2 || ', ' || (1-sqrt(2))/2 || ' ' || (1+sqrt(2))/2 || ', 0.25 0.75, 0 0.5, 0.25 0.25, 0 0))'
  );


INSERT INTO public.puzzles (color, shape) -- puzzle 2
SELECT
  'black',
  ST_GeomFromText(
    'POLYGON((0 0, 1 0, 0.75 0.25, 1 0.5, 0.75 0.75, ' || 0.5 + (sqrt(2)/2) || ' 1.25, 0.75 1.25, 0.5 1, 0.25 1.25, -0.25 1.25, 0.25 0.75, 0 0.5, 0.25 0.25, 0 0))'
  );


INSERT INTO public.puzzles (color, shape) -- puzzle 3
SELECT
  'black',
  ST_GeomFromText('POLYGON((0 0, ' || ((3*sqrt(2))/4) || ' 0, '|| ((3*sqrt(2)+2)/8) || ' ' || (3*sqrt(2)-2)/8 || ', ' || ((3*sqrt(2)+4)/8) || ' ' || ((3*sqrt(2))/8) || ', '|| ((3*sqrt(2)+2)/8) || ' ' || ((3*sqrt(2)+2)/8) || ', ' || ((7*sqrt(2))/8) || ' ' || (7*sqrt(2))/8 || ', ' || (-sqrt(2)/8) || ' ' || ((7*sqrt(2))/8) || ', ' || ((3*sqrt(2)-2)/8) || ' ' || ((3*sqrt(2)+2)/8) || ', ' || ( (3*sqrt(2)-4) / 8) || ' ' || ((3*sqrt(2))/8) || ', ' || (3*sqrt(2)-2)/8 || ' ' || ((3*sqrt(2)-2)/8) || ', 0 0), (' || (sqrt(2)/2) || ' ' || (sqrt(2)/4) || ', ' || ((3*sqrt(2))/8) || ' ' || ((3*sqrt(2))/8) || ', ' || ((sqrt(2))/4) || ' ' || (sqrt(2))/4 || ', ' || (sqrt(2)/2) || ' ' || (sqrt(2)/4) || '))');



-- Puzzle public.solver

-- Puzzle 1 solution
INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- red
  (1, 1,
  ST_GeomFromText('POINT(0.5 0.5)'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- orange
  (1, 2,
  ST_GeomFromText('POINT('|| (1+sqrt(2))/2 || ' ' || (1+sqrt(2))/2 || ')'),
  1
  );

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- blue
  (1, 3,
  ST_GeomFromText('POINT(0.25 0.25)'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- pink
  (1, 4,
  ST_GeomFromText('POINT(0.75 0.75)'),
  1.5);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- purple
  (1, 5,
  ST_GeomFromText('POINT(0 0)'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- yellow
  (1, 6,
  ST_GeomFromText('POINT(0.25 0.25)'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- green
  (1, 7,
  ST_GeomFromText('POINT(0.5 0)'),
  0);

-- Puzzle 2 solution
INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- red
  (2, 1,
  ST_GeomFromText('POINT(1 0)'),
  0.25);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- orange
  (2, 2,
  ST_GeomFromText('POINT(0.75 1.25)'),
  1.25);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- blue
  (2, 3,
  ST_GeomFromText('POINT(1.25 1.25)'),
  1);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- pink
  (2, 4,
  ST_GeomFromText('POINT(0.5 1)'),
  1);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- purple
  (2, 5,
  ST_GeomFromText('POINT(0.75 0.75)'),
  1.5);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- yellow
  (2, 6,
  ST_GeomFromText('POINT(0.25 0.25)'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- green
  (2, 7,
  ST_GeomFromText('POINT(0 1)'),
  0);

-- Puzzle 3 solution
INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- red
  (3, 1,
  ST_GeomFromText('POINT(' || ((3*sqrt(2)+4)/8) - 0.5 || ' ' || (3*sqrt(2))/8 || ')'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- orange
  (3, 2,
  ST_GeomFromText('POINT(' || ((7*sqrt(2))/8) || ' ' || ((7*sqrt(2))/8) ||')'),
  1);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES
  (3, 3, -- blue
  ST_GeomFromText('POINT(' || ((3*sqrt(2))/4) || ' 0)'),
  0.75);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- pink
  (3, 4,
  ST_GeomFromText('POINT(' || ((3*sqrt(2)+4)/8) - 0.5 || ' ' || (3*sqrt(2))/8 || ')'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- purple
  (3, 5,
  ST_GeomFromText('POINT(' || ((3*sqrt(2)+4)/8) ||  ' ' || (3*sqrt(2))/8 || ')'),
  1);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- yellow
  (3, 6,
  ST_GeomFromText('POINT(' || (3*sqrt(2)-2)/8 || ' ' || (3*sqrt(2)-2)/8 ||')'),
  0);

INSERT INTO public.solver (id_puzzle, id_piece, pT, pR)
VALUES -- green
  (3, 7,
  ST_GeomFromText('POINT(' || sqrt(2)/4 || ' 0)'),
  0.25);
