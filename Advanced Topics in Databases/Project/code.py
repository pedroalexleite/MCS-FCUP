# ython3 plot.py

import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import psycopg2
import math
from shapely.wkt import loads

conn = psycopg2.connect(
    host="localhost",
    database="database",
    user="pedroalexleite",
    password="password"
)

while True:
    number = input("Enter a number (1 - Pieces, 2 - Puzzles, 3 - Solvers, q - Quit): ")
    if number == '1' or number == '2' or number == '3':
        number = int(number)
    if number == 'q':
        break
    elif number == '1' or number == '2' or number == '3':
        print("Error. Number not between 1 and 3")
        while True:
            number = input("Enter a number (1 - Pieces, 2 - Puzzles, 3 - Solvers, q - Quit): ")
            if number == '1' or number == '2' or number == '3':
                number = int(number)
                break
            elif number == 'q':
                break
            else:
                print("Error. Number not between 1 and 3")

    cursor = conn.cursor()

    if number == 1:
        sub_number = input("Enter a number (1 - Red Large Triangle, 2 - Orange Large Triangle, 3 - Blue Medium Triangle, 4 - Pink Small Triangle, 5 - Purple Small Triangle, 6 - Yellow Square, 7 - Green Parallelogram): ")
        if sub_number == '1' or sub_number == '2' or sub_number == '3' or sub_number == '4' or sub_number == '5' or sub_number == '6' or sub_number == '7':
            sub_number = int(sub_number)
            query = "SELECT ST_AsText(shape), color FROM public.pieces WHERE id={}".format(sub_number)
            cursor.execute(query)
            shapes_and_colors = cursor.fetchall()
        else:
            print("Error. Number not between 1 and 7")
            while True:
                sub_number = input("Enter a number (1 - Red Large Triangle, 2 - Orange Large Triangle, 3 - Blue Medium Triangle, 4 - Pink Small Triangle, 5 - Purple Small Triangle, 6 - Yellow Square, 7 - Green Parallelogram): ")
                if sub_number == '1' or sub_number == '2' or sub_number == '3' or sub_number == '4' or sub_number == '5' or sub_number == '6' or sub_number == '7':
                    sub_number = int(sub_number)
                    query = "SELECT ST_AsText(shape), color FROM public.pieces WHERE id={}".format(sub_number)
                    cursor.execute(query)
                    shapes_and_colors = cursor.fetchall()
                    break
                else:
                    print("Error. Number not between 1 and 7")

    elif number == 2:
        sub_number = input("Enter a number (1 - Puzzle 1, 2 - Puzzle 2, 3 - Puzzle 3): ")
        if sub_number == '1' or sub_number == '2' or sub_number == '3':
            sub_number = int(sub_number)
            query = "SELECT ST_AsText(shape), color FROM public.puzzles WHERE id={}".format(sub_number)
            cursor.execute(query)
            shapes_and_colors = cursor.fetchall()
        else:
            print("Error. Number not between 1 and 3")
            while True:
                sub_number = input("Enter a number (1 - Puzzle 1, 2 - Puzzle 2, 3 - Puzzle 3): ")

                if sub_number == '1' or sub_number == '2' or sub_number == '3':
                    sub_number = int(sub_number)
                    query = "SELECT ST_AsText(shape), color FROM public.puzzles WHERE id={}".format(sub_number)
                    cursor.execute(query)
                    shapes_and_colors = cursor.fetchall()
                    break
                else:
                    print("Error. Number not between 1 and 3")

    elif number == 3:
        sub_number = input("Enter a number (1 - Solver 1, 2 - Solver 2, 3 - Solver 3): ")
        if sub_number == '1':
            cursor.execute("SELECT pieces.id, pieces.shape, pieces.color, solver.pR, ST_X(solver.pT::geometry) AS translation_x, ST_Y(solver.pT::geometry) AS translation_y FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE id_puzzle = 1")
        elif sub_number == '2':
            cursor.execute("SELECT pieces.id, pieces.shape, pieces.color, solver.pR, ST_X(solver.pT::geometry) AS translation_x, ST_Y(solver.pT::geometry) AS translation_y FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE id_puzzle = 2")
        elif sub_number == '3':
            cursor.execute("SELECT pieces.id, pieces.shape, pieces.color, solver.pR, ST_X(solver.pT::geometry) AS translation_x, ST_Y(solver.pT::geometry) AS translation_y FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE id_puzzle = 3")
        else:
            print("Error. Number not between 1 and 3")
            while True:
                sub_number = input("Enter a number (1 - Solver 1, 2 - Solver 2, 3 - Solver 3): ")

                if sub_number == '1':
                    cursor.execute("SELECT pieces.id, pieces.shape, pieces.color, solver.pR, ST_X(solver.pT::geometry) AS translation_x, ST_Y(solver.pT::geometry) AS translation_y FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE id_puzzle = 1")
                    break
                elif sub_number == '2':
                    cursor.execute("SELECT pieces.id, pieces.shape, pieces.color, solver.pR, ST_X(solver.pT::geometry) AS translation_x, ST_Y(solver.pT::geometry) AS translation_y FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE id_puzzle = 2")
                    break
                elif sub_number == '3':
                    cursor.execute("SELECT pieces.id, pieces.shape, pieces.color, solver.pR, ST_X(solver.pT::geometry) AS translation_x, ST_Y(solver.pT::geometry) AS translation_y FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE id_puzzle = 3")
                    break
                else:
                    print("Error. Number not between 1 and 3")

        shapes_and_colors = cursor.fetchall()

        fig, ax = plt.subplots()

        for piece_id, shape, color, solver_pR, translation_x, translation_y in shapes_and_colors:
            cursor.execute("SELECT ST_AsText(ST_Translate(ST_Rotate(%s, %s, 0, 0), %s, %s)) FROM public.solver AS solver INNER JOIN public.pieces as pieces ON pieces.id = solver.id_piece WHERE pieces.id = %s",
                           (shape, math.pi * solver_pR, translation_x, translation_y, piece_id))

            transformed_shape = cursor.fetchone()[0]

            shapely_polygon = loads(transformed_shape)

            x, y = shapely_polygon.exterior.xy

            ax.fill(x, y, color=color)

        ax.set_xlim(-0.5, 1.5)
        ax.set_ylim(-0.5, 1.5)

        plt.show()

        cursor.close()
    else:
        print("Error. Number not between 1 and 3")

    if number == 2 and sub_number == 3:
        cursor.close()

        fig, ax = plt.subplots()

        geometry_wkb = shapes_and_colors[0][0]

        geometry = loads(geometry_wkb)

        x_exterior, y_exterior = geometry.exterior.xy

        x_interior, y_interior = geometry.interiors[0].xy

        plt.fill(x_exterior, y_exterior, 'black')

        plt.fill(x_interior, y_interior, 'white')

        ax.set_xlim(-0.5, 1.5)
        ax.set_ylim(-0.5, 1.5)

        plt.show()

    elif (number == 1) or (number == 2 and sub_number == 1) or (number == 2 and sub_number == 2):
        cursor.close()

        fig, ax = plt.subplots()

        for shape, color in shapes_and_colors:
            ax.fill(*loads(shape).exterior.xy, color=color)

        if number == 1:
            ax.set_xlim(-1, 1)
            ax.set_ylim(-1, 1)
        elif number == 2:
            ax.set_xlim(-0.5, 1.5)
            ax.set_ylim(-0.5, 1.5)

        plt.show()

conn.close()
